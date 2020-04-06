module Components.TypeaheadHook where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AR
import Components.DropdownHook as D
import Data.Argonaut.Decode ((.:), decodeJson)
import Data.Array (mapWithIndex, filter, (:), (!!), length, null, difference)
import Data.Bifunctor (bimap)
import Data.Const (Const)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Halogen (liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (useState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks.UseEvent (subscribeTo)
import Internal.CSS (class_, classes_, whenElem)
import Internal.RemoteData as RD
import Select (useSelect)
import Select as S

data Query a
  = GetSelections (Array Location -> a)

data Message
  = ItemRemoved Location
  | SelectionsChanged (Array Location)

type ChildSlots =
  ( dropdown :: D.Slot (Const Void) Unit )

component :: H.Component HH.HTML Query Unit Message Aff
component = Hooks.componentWithQuery \queryToken _ -> Hooks.do
  selections /\ tSelections <- useState []
  available /\ tAvailable <- useState RD.NotAsked

  select <- useSelect { inputType: S.Text
                      , debounceTime: Just (Milliseconds 300.0)
                      , search: Nothing
                      , getItemCount: pure $ maybe 0 length $ RD.toMaybe available
                      }

  subscribeTo select.onSelectedIdxChanged \ix -> do
    available' <- Hooks.get tAvailable
    for_ available' \arr ->
      for_ (arr !! ix) \item -> do
        selections' <- Hooks.get tSelections
        let newSelections = item : selections'
        Hooks.put tAvailable (RD.Success (filter (_ /= item) arr))
        Hooks.put tSelections newSelections
        select.clearSearch
        Hooks.raise $ SelectionsChanged newSelections

  subscribeTo select.onNewSearch \str -> do
    selections' <- Hooks.get tSelections
    -- we'll use an external api to search locations
    Hooks.put tAvailable RD.Loading
    items <- liftAff $ searchLocations str
    Hooks.put tAvailable $ items <#> \xs -> difference xs selections'

  Hooks.useQuery queryToken case _ of
    GetSelections reply -> do
       selections' <- Hooks.get tSelections
       pure $ Just $ reply selections'

  Hooks.pure $
    HH.div
      [ class_ "Typeahead" ]
      [ renderSelections selections tSelections
      , renderInput select selections
      , renderDropdown select tSelections
      , renderContainer select selections available
      ]
  where
  remove tSelections item = do
    selections <- Hooks.get tSelections
    let newSelections = filter (_ /= item) selections
    Hooks.put tSelections newSelections
    Hooks.raise $ ItemRemoved item

  handleDropdown tSelections msg = case msg of
    D.SelectionChanged oldSelection newSelection -> do
      selections <- Hooks.get tSelections
      let
        mkLocation str = { name: "User Added: " <> str, population: "1" }
        newSelections = case oldSelection, newSelection of
          Nothing, Nothing ->
            Nothing
          Nothing, Just str ->
            Just (mkLocation str : selections)
          Just str, Nothing ->
            Just (filter (_ /= mkLocation str) selections)
          Just old, Just new ->
            Just (mkLocation new : (filter (_ /= mkLocation old) selections))
      for_ newSelections \selections' ->
        Hooks.put tSelections selections'

  renderSelections selections tSelections =
    whenElem (length selections > 0) \_ ->
      HH.div
        [ class_ "Typeahead__selections" ]
        (renderSelectedItem <$> selections)
      where
      renderSelectedItem item =
        HH.div
          [ class_ "Typeahead__item--selected Location" ]
          [ HH.span
              [ class_ "Location__name" ]
              [ HH.text item.name ]
          , closeButton item
          ]

      closeButton item =
        HH.span
          [ class_ "Location__closeButton"
          , HE.onClick \_ -> Just $ remove tSelections item
          ]
          [ HH.text "×" ]

  renderInput select selections =
    HH.input
      (select.inputProps <>
        [ classes_
            [ "Typeahead__input"
            , "Typeahead__input--selections" # guard (length selections > 0)
            , "Typeahead__input--active"
                # guard (select.visibility == S.On)
            ]
        , HP.placeholder "Type to search..."
        ])

  renderDropdown select tSelections =
    whenElem (select.visibility == S.On) \_ ->
      HH.slot _dropdown unit D.component dropdownInput handler
    where
    _dropdown = SProxy :: SProxy "dropdown"
    handler msg = Just $ handleDropdown tSelections msg
    dropdownInput = { items: [ "Earth", "Mars" ], buttonLabel: "Human Planets" }

  renderContainer select selections available =
    whenElem (select.visibility == S.On) \_ ->
      HH.div
        (select.containerProps <>
          [ classes_
              [ "Typeahead__container"
              , "Typeahead__container--hasItems" # guard hasItems
              ]
          ]
        )
        renderItems
      where
      hasItems = maybe false (not <<< null) (RD.toMaybe available)
      renderItems = do
        let renderMsg msg = [ HH.span_ [ HH.text msg ] ]
        case available of
          RD.NotAsked -> renderMsg "No search performed..."
          RD.Loading -> renderMsg "Loading..."
          RD.Failure e -> renderMsg e
          RD.Success available'
            | length selections > 0 -> renderItem `mapWithIndex` available'
            | otherwise -> renderMsg "No results found"

      renderItem index { name, population } =
        HH.div
          ((select.itemProps index) <>
            [ classes_ [ base, highlight, "Location" ] ])
          [ HH.span
              [ class_ "Location__name" ]
              [ HH.text name ]
          , HH.span
              [ class_ "Location__population" ]
              [ HH.text population ]
          ]
        where
        base = "Typeahead__item"
        highlight = "Typeahead__item--highlighted"
                      # guard (select.highlightedIndex == Just index)


-- Let's make this typeahead async.

type Location =
  { name :: String
  , population :: String
  }

searchLocations :: String -> Aff (RD.RemoteData String (Array Location))
searchLocations search = do
  eitherRes <- AX.get AR.json ("https://swapi.co/api/planets/?search=" <> search)
  let body = bimap AX.printError (_.body) eitherRes
  pure $ RD.fromEither $ traverse decodeJson =<< (_ .: "results") =<< decodeJson =<< body
