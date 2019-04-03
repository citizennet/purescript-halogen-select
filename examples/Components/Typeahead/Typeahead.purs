module Docs.Components.Typeahead where

import Prelude

import Data.Array (elemIndex, mapWithIndex, difference, filter, (:))
import Data.Const (Const)
import Data.Foldable (for_, length)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.String (Pattern(..), contains)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj, match)
import Docs.CSS as CSS
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select as Select
import Select.Setters as Setters

type TypeaheadItem = String

data Action
  = Remove TypeaheadItem
  | HandleSelect (Select.Message TypeaheadItem ())

type State =
  { items :: Array TypeaheadItem
  , selected :: Array TypeaheadItem
  , keepOpen :: Boolean 
  }

type Input = 
  { items :: Array String
  , keepOpen :: Boolean 
  }

type Query = Const Void

type Message = Void

-----
-- Extension

type ChildSlots m = 
  ( select :: H.Slot (Select.Query TypeaheadItem ExtraActions () m) (Select.Message TypeaheadItem ()) Unit)

_select = SProxy :: SProxy "select"

-- Extending `Select`
type ExtraActions = ( log :: String )

log :: forall m. String -> Select.Action TypeaheadItem ExtraActions () m
log = Select.Action <<< inj (SProxy :: _ "log")

component :: ∀ m. MonadAff m => H.Component HH.HTML Query Input Message m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  initialState :: Input -> State
  initialState i = { items: i.items, selected: [], keepOpen: i.keepOpen }

  render :: State -> H.ComponentHTML Action (ChildSlots m) m
  render st =
    HH.div
      [ class_ "w-full" ]
      [ renderSelections st.selected
      , HH.slot _select unit (Select.component handleExtraActions) input (Just <<< HandleSelect)
      ]
    where
    input =
      { initialSearch: Nothing
      , debounceTime: Nothing
      , inputType: Select.TextInput
      , items: difference st.items st.selected
      , render: renderSelect
      }

  handleExtraActions 
    :: Variant ExtraActions 
    -> H.HalogenM _ (Select.Action TypeaheadItem ExtraActions () m) () _ m Unit
  handleExtraActions = match  
    { log: \str -> Console.log str 
    }

  handleAction :: Action -> H.HalogenM State Action (ChildSlots m) Message m Unit
  handleAction = case _ of
    HandleSelect msg -> match
      { searched: \search -> do
          st <- H.get
          let newItems = difference (filterItems search st.items) st.selected
              index = elemIndex search st.items
          _ <- H.query _select unit $ H.tell $ Select.ReplaceItems newItems
          for_ index \ix -> 
            H.query _select unit $ H.tell $ Select.PerformAction $ Select.highlight $ Select.Index ix
      
      , selected: \item -> do
          st <- H.get

          when (not st.keepOpen) do
            void $ H.query _select unit $ H.tell $ Select.PerformAction $ Select.setVisibility Select.Off

          case length (filter ((==) item) st.items) of
            0 ->
              H.modify_ _ { items = ( item : st.items ), selected = ( item : st.selected ) }
            _ ->
              H.modify_ _ { selected = ( item : st.selected ) }

          newSt <- H.get
          let newItems = difference newSt.items newSt.selected
          void $ H.query _select unit $ H.tell $ Select.ReplaceItems newItems
      
      , visibilityChanged: \_ -> pure unit
      } msg

    Remove item -> do
      newSt <- H.modify \oldSt -> oldSt { selected = filter ((/=) item) oldSt.selected }
      void $ H.query _select unit $ H.tell $ Select.ReplaceItems $ difference newSt.items newSt.selected


----------
-- Helpers

class_ :: ∀ p i. String -> HH.IProp ( "class" :: String | i ) p
class_ = HP.class_ <<< HH.ClassName

filterItems :: TypeaheadItem -> Array TypeaheadItem -> Array TypeaheadItem
filterItems str = filter (\i -> contains (Pattern str) i)

renderSelect 
  :: forall m
   . Select.State TypeaheadItem 
  -> H.ComponentHTML (Select.Action TypeaheadItem ExtraActions () m) () m
renderSelect state = HH.div_ [ renderInput, renderContainer ]
  where
    renderInput = HH.input $ Setters.setInputProps
      [ HP.classes CSS.input
      , HP.placeholder "Type to search..." 
      ]

    renderContainer =
      HH.div 
        [ class_ "relative z-50" ]
        if state.visibility == Select.Off
          then []
          else [ renderItems $ renderItem `mapWithIndex` state.items ]
      where
      renderChild =
        HH.div
          [ HE.onClick \_ -> Just $ log "I was clicked" ]
          [ HH.text "CLICK ME I'M FROM THE PARENT" ]

      renderItems html =
        HH.div
          ( Setters.setContainerProps
            [ class_ "absolute bg-white shadow rounded-sm pin-t pin-l w-full" ]
          )
          [ renderChild
          , HH.ul 
              [ class_ "list-reset" ] 
              html 
          ]

      renderItem index item =
        HH.li ( Setters.setItemProps index [ class_ classString ] ) [ HH.text item ]
        where
        classString = 
          "px-4 py-1 text-grey-darkest" <> " bg-grey-lighter" # guard (state.highlightedIndex == Just index) 

renderSelections :: forall props. Array TypeaheadItem -> HH.HTML props Action
renderSelections items = case length items of
  0 -> 
    HH.div_ []
  _ ->
    HH.div
      [ class_ "bg-white rounded-sm w-full border-b border-grey-lighter" ]
      [ HH.ul
        [ class_ "list-reset" ]
        (renderSelectedItem <$> items)
      ]
  where
  renderSelectedItem item =
    HH.li
      [ class_ "px-4 py-1 text-grey-darkest hover:bg-grey-lighter relative" ]
      [ HH.span_ [ HH.text item ]
      , closeButton item
      ]

  closeButton item =
    HH.span
      [ HE.onClick \_ -> Just (Remove item)
      , class_ "absolute pin-t pin-b pin-r p-1 mx-3 cursor-pointer" 
      ]
      [ HH.text "×" ]
