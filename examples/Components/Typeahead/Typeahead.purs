module Docs.Components.Typeahead where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AR
import Data.Argonaut.Decode ((.:), decodeJson)
import Data.Array (mapWithIndex, filter, (:), (!!))
import Data.Foldable (for_, length)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Docs.CSS as CSS
import Docs.Internal.RemoteData as RD
import Docs.Components.Dropdown as D
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select as S
import Select.Setters as SS

type ExtraState =
  ( selections :: Array Location 
  , available :: RD.RemoteData String (Array Location)
  )

data ExtraAction
  = Remove Location
  | HandleDropdown D.ExtraMessage

data ExtraQuery a
  = GetSelections (Array Location -> a)

data ExtraMessage 
  = ItemRemoved Location 
  | SelectionsChanged (Array Location)

type ChildSlots = 
  ( dropdown :: D.Slot Unit )

type Slot = 
  S.Slot ExtraQuery ChildSlots ExtraMessage

spec :: S.Spec ExtraState ExtraQuery ExtraAction ChildSlots ExtraMessage Aff
spec = S.defaultSpec 
  { render = render
  , handleAction = handleAction
  , handleQuery = handleQuery
  , handleMessage = handleMessage
  }
  where
  handleMessage = case _ of
    S.Selected ix -> do
      st <- H.get
      for_ st.available \arr -> do
        let newSelections = fromMaybe st.selections $ (_ : st.selections) <$> (arr !! ix)
        H.modify_ _ { selections = newSelections, search = "" } 
        H.raise $ SelectionsChanged arr
  
    S.Searched str -> do
      st <- H.get
      -- we'll use an external api to search locations 
      H.modify_ _ { available = RD.Loading }
      items <- H.liftAff $ searchLocations str
      H.modify_ _ { available = items }
    _ -> pure unit

  -- type signature is necessary for the `a` type variable 
  handleQuery :: forall a. ExtraQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of  
    GetSelections reply -> do
       st <- H.get
       pure $ Just $ reply st.selections

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

  render state = HH.div_ [ renderSelections, renderInput, renderContainer ]
    where
    renderSelections = case length state.selections of
      0 -> 
        HH.div_ []
      _ ->
        HH.div
          [ class_ "bg-white rounded-sm w-full border-b border-grey-lighter" ]
          [ HH.ul [ class_ "list-reset" ] (renderSelectedItem <$> state.selections) ]
      where
      renderSelectedItem item =
        HH.li
          [ class_ "px-4 py-1 text-grey-darkest hover:bg-grey-lighter relative" ]
          [ renderLocation item, closeButton item ]

      closeButton item =
        HH.span
          [ HE.onClick \_ -> Just $ S.Action $ Remove item
          , class_ "absolute pin-t pin-b pin-r p-1 mx-3 cursor-pointer" 
          ]
          [ HH.text "×" ]

    renderInput = HH.input $ SS.setInputProps
      [ HP.classes CSS.input
      , HP.placeholder "Type to search..." 
      ]

    renderContainer =
      HH.div 
        [ class_ "relative z-50" ]
        ([ renderItems (mapWithIndex renderItem) ] # guard (state.visibility == S.On))
      where
      -- here we can render a further child component, the dropdown, which is *also*
      -- a select component.
      renderChild = HH.slot _dropdown unit (S.component D.spec) input handleChild
        where
        _dropdown = SProxy :: SProxy "dropdown"
        handleChild msg = Just $ S.Action $ HandleDropdown msg
        input = 
          { inputType: S.Toggle
          , search: Nothing
          , debounceTime: Nothing
          , getItemCount: length <<< _.items
          , items: [ "Earth", "Mars" ]
          , selection: Nothing
          }

      renderItems f = do
        let renderMsg msg = [ HH.span [ class_ "pa-4" ] [ HH.text msg ] ]
        HH.div
          (SS.setContainerProps 
            [ class_ "absolute bg-white shadow rounded-sm pin-t pin-l w-full" ]
          )
          case state.available of
            RD.NotAsked -> renderMsg "No search performed..."
            RD.Loading -> renderMsg "Loading..."
            RD.Failure e -> renderMsg e
            RD.Success available -> 
              [ HH.ul 
                  [ class_ "list-reset" ] 
                  (f available) 
              , renderChild
              ]

      renderItem index item =
        HH.li 
          (SS.setItemProps index [ class_ $ base <> extra ]) 
          [ renderLocation item ]
        where
        base = "px-4 py-1 text-grey-darkest"
        extra = " bg-grey-lighter" # guard (state.highlightedIndex == Just index) 

-- Helpers

class_ :: ∀ p i. String -> HH.IProp ( "class" :: String | i ) p
class_ = HP.class_ <<< HH.ClassName

type Location =
  { name :: String
  , population :: String
  }

renderLocation :: forall t0 t1. Location -> HH.HTML t0 t1
renderLocation { name, population } = 
  HH.div_
    [ HH.text name
    , HH.span
        [ class_ "ml-4 text-grey-lighter" ]
        [ HH.text population ]
    ]

searchLocations :: String -> Aff (RD.RemoteData String (Array Location))
searchLocations search = do
  res <- AX.get AR.json ("https://swapi.co/api/planets/?search=" <> search) 
  let body = lmap AR.printResponseFormatError res.body
  pure $ RD.fromEither $ traverse decodeJson =<< (_ .: "results") =<< decodeJson =<< body

