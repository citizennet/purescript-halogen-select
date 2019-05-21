module Components.Typeahead where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AR
import Components.Dropdown as D
import Data.Argonaut.Decode ((.:), decodeJson)
import Data.Array (mapWithIndex, filter, (:), (!!), length, null, difference)
import Data.Foldable (for_)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Internal.CSS (class_, classes_, whenElem)
import Internal.RemoteData as RD
import Select as S
import Select.Setters as SS

type Slot =
  S.Slot Query ChildSlots Message

type State =
  ( selections :: Array Location
  , available :: RD.RemoteData String (Array Location)
  )

data Action
  = Remove Location
  | HandleDropdown D.Message

data Query a
  = GetSelections (Array Location -> a)

data Message
  = ItemRemoved Location
  | SelectionsChanged (Array Location)

type ChildSlots =
  ( dropdown :: D.Slot Unit )

component :: H.Component HH.HTML (S.Query Query ChildSlots) Unit Message Aff
component = S.component (const input) $ S.defaultSpec
  { render = render
  , handleAction = handleAction
  , handleQuery = handleQuery
  , handleMessage = handleMessage
  }
  where
  -- this typeahead will be opaque; users can just use this pre-built
  -- input instead of the usual select one.
  input :: S.Input State
  input =
    { inputType: S.Text
    , debounceTime: Just (Milliseconds 300.0)
    , search: Nothing
    , getItemCount: maybe 0 length <<< RD.toMaybe <<< _.available
    , selections: []
    , available: RD.NotAsked
    }

  handleMessage
    :: S.Message
    -> H.HalogenM (S.State State) (S.Action Action) ChildSlots Message Aff Unit
  handleMessage = case _ of
    S.Selected ix -> do
      st <- H.get
      for_ st.available \arr ->
        for_ (arr !! ix) \item -> do
          let newSelections = item : st.selections
          H.modify_ _
            { selections = item : st.selections
            , available = RD.Success (filter (_ /= item) arr)
            , search = ""
            }
          H.raise $ SelectionsChanged newSelections
    S.Searched str -> do
      st <- H.get
      -- we'll use an external api to search locations
      H.modify_ _ { available = RD.Loading }
      items <- H.liftAff $ searchLocations str
      H.modify_ _ { available = items <#> \xs -> difference xs st.selections }
    _ -> pure unit

  -- You can remove all type signatures except for this one; we need to tell the
  -- compiler about the `a` type variable. The minimal necessary signature is below.
  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    GetSelections reply -> do
       st <- H.get
       pure $ Just $ reply st.selections

  handleAction
    :: Action
    -> H.HalogenM (S.State State) (S.Action Action) ChildSlots Message Aff Unit
  handleAction = case _ of
    Remove item -> do
      st <- H.get
      let newSelections = filter (_ /= item) st.selections
      H.modify_ _ { selections = newSelections }
      H.raise $ ItemRemoved item
    HandleDropdown msg -> case msg of
      D.SelectionChanged oldSelection newSelection -> do
        st <- H.get
        let
          mkLocation str = { name: "User Added: " <> str, population: "1" }
          newSelections = case oldSelection, newSelection of
            Nothing, Nothing ->
              Nothing
            Nothing, Just str ->
              Just (mkLocation str : st.selections)
            Just str, Nothing ->
              Just (filter (_ /= mkLocation str) st.selections)
            Just old, Just new ->
              Just (mkLocation new : (filter (_ /= mkLocation old) st.selections))
        for_ newSelections \selections ->
          H.modify_ _ { selections = selections }

  render :: S.State State -> H.ComponentHTML (S.Action Action) ChildSlots Aff
  render st =
    HH.div
      [ class_ "Typeahead" ]
      [ renderSelections, renderInput, renderDropdown, renderContainer ]
    where
    hasSelections = length st.selections > 0

    renderSelections = whenElem hasSelections \_ ->
      HH.div
        [ class_ "Typeahead__selections" ]
        (renderSelectedItem <$> st.selections)
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
          , HE.onClick \_ -> Just $ S.Action $ Remove item
          ]
          [ HH.text "×" ]

    renderInput = HH.input $ SS.setInputProps
      [ classes_
          [ "Typeahead__input"
          , "Typeahead__input--selections" # guard hasSelections
          , "Typeahead__input--active" # guard (st.visibility == S.On)
          ]
      , HP.placeholder "Type to search..."
      ]

    renderDropdown = whenElem (st.visibility == S.On) \_ ->
      HH.slot _dropdown unit D.component dropdownInput handler
      where
      _dropdown = SProxy :: SProxy "dropdown"
      handler msg = Just $ S.Action $ HandleDropdown msg
      dropdownInput = { items: [ "Earth", "Mars" ], buttonLabel: "Human Planets" }

    renderContainer = whenElem (st.visibility == S.On) \_ ->
      HH.div
        (SS.setContainerProps
          [ classes_
              [ "Typeahead__container"
              , "Typeahead__container--hasItems" # guard hasItems
              ]
          ]
        )
        renderItems
      where
      hasItems = maybe false (not <<< null) (RD.toMaybe st.available)
      renderItems = do
        let renderMsg msg = [ HH.span_ [ HH.text msg ] ]
        case st.available of
          RD.NotAsked -> renderMsg "No search performed..."
          RD.Loading -> renderMsg "Loading..."
          RD.Failure e -> renderMsg e
          RD.Success available
            | hasItems -> renderItem `mapWithIndex` available
            | otherwise -> renderMsg "No results found"

      renderItem index { name, population } =
        HH.div
          (SS.setItemProps index [ classes_ [ base, highlight, "Location" ] ])
          [ HH.span
              [ class_ "Location__name" ]
              [ HH.text name ]
          , HH.span
              [ class_ "Location__population" ]
              [ HH.text population ]
          ]
        where
        base = "Typeahead__item"
        highlight = "Typeahead__item--highlighted" # guard (st.highlightedIndex == Just index)


-- Let's make this typeahead async.

type Location =
  { name :: String
  , population :: String
  }

searchLocations :: String -> Aff (RD.RemoteData String (Array Location))
searchLocations search = do
  res <- AX.get AR.json ("https://swapi.co/api/planets/?search=" <> search)
  let body = lmap AR.printResponseFormatError res.body
  pure $ RD.fromEither $ traverse decodeJson =<< (_ .: "results") =<< decodeJson =<< body
