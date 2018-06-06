module Docs.Components.Typeahead where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Data.Array (elemIndex, mapWithIndex, difference, filter, (:))
import Data.Foldable (length, traverse_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Select as Select
import Select.Utils.Setters as Setters

import Docs.CSS as CSS

type TypeaheadItem = String

data Query a
  = Log String a
  | HandleInputContainer (Select.Message Query TypeaheadItem) a
  | Removed TypeaheadItem a

type State =
  { items    :: Array TypeaheadItem
  , selected :: Array TypeaheadItem
  , keepOpen :: Boolean }

type Input = { items :: Array String, keepOpen :: Boolean }
data Message = Void

type ChildSlot = Unit
type ChildQuery = Select.Query Query TypeaheadItem

component :: ∀ m. MonadAff m => H.Component HH.HTML Query Input Message m
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: Input -> State
    initialState i = { items: i.items, selected: [], keepOpen: i.keepOpen }

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
    render st =
      HH.div
        [ class_ "w-full" ]
        [ renderSelections st.selected
        , HH.slot unit Select.component input (HE.input HandleInputContainer)
        ]
      where
        input =
          { initialSearch: Nothing
          , debounceTime: Nothing
          , inputType: Select.TextInput
          , items: difference st.items st.selected
          , render: renderInputContainer
          }

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message m
    eval = case _ of
      Log str a -> pure a

      HandleInputContainer m a -> a <$ case m of
        Select.Emit q -> eval q

        Select.Searched search -> do
          st <- H.get
          let newItems = difference (filterItems search st.items) st.selected
              index = elemIndex search st.items
          _ <- H.query unit $ Select.replaceItems newItems
          traverse_ (H.query unit <<< Select.highlight <<< Select.Index) index

        Select.Selected item -> do
          st <- H.get

          _ <- if st.keepOpen
            then pure unit
            else do
              _ <- H.query unit $ Select.setVisibility Select.Off
              pure unit

          if length (filter ((==) item) st.items) > 0
            then H.modify_ _ { selected = ( item : st.selected ) }
            else H.modify_ _
                  { items = ( item : st.items )
                  , selected = ( item : st.selected ) }

          newSt <- H.get
          let newItems = difference newSt.items newSt.selected
          _ <- H.query unit $ Select.replaceItems newItems
          pure unit

        otherwise -> pure unit

      Removed item a -> do
        st <- H.get
        H.modify_ _ { selected = filter ((/=) item) st.selected }
        newSt <- H.get
        let newItems = difference newSt.items newSt.selected
        _ <- H.query unit $ Select.replaceItems newItems
        pure a


----------
-- Helpers

class_ :: ∀ p i. String -> H.IProp ( "class" :: String | i ) p
class_ = HP.class_ <<< HH.ClassName

filterItems :: TypeaheadItem -> Array TypeaheadItem -> Array TypeaheadItem
filterItems str = filter (\i -> contains (Pattern str) i)

renderInputContainer :: Select.State TypeaheadItem -> Select.ComponentHTML Query TypeaheadItem
renderInputContainer state = HH.div_ [ renderInput, renderContainer ]
  where
    renderInput = HH.input $ Setters.setInputProps
      [ HP.classes CSS.input
      , HP.placeholder "Type to search..." ]

    renderContainer =
      HH.div [ class_ "relative z-50" ]
      $ if state.visibility == Select.Off
        then []
        else [ renderItems $ renderItem `mapWithIndex` state.items ]
      where
        renderChild =
          HH.div
          [ HE.onClick $ Select.always $ Select.raise $ H.action $ Log "I was clicked" ]
          [ HH.text "CLICK ME I'M FROM THE PARENT" ]

        renderItems html =
          HH.div
          ( Setters.setContainerProps
            [ class_ "absolute bg-white shadow rounded-sm pin-t pin-l w-full" ]
          )
          [ renderChild, HH.ul [ class_ "list-reset" ] html ]

        renderItem index item =
          HH.li ( Setters.setItemProps index props ) [ HH.text item ]
          where
            props = [ class_
              $ "px-4 py-1 text-grey-darkest"
              <> if state.highlightedIndex == Just index
                   then " bg-grey-lighter"
                   else "" ]

renderSelections
  :: ∀ p
   . Array TypeaheadItem
  -> H.HTML p Query
renderSelections items =
  if length items == 0
    then HH.div_ []
    else
    HH.div
    [ class_ "bg-white rounded-sm w-full border-b border-grey-lighter" ]
    [ HH.ul
      [ class_ "list-reset" ]
      ( renderSelectedItem <$> items )
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
      [ HE.onClick $ HE.input_ (Removed item)
      , class_ "absolute pin-t pin-b pin-r p-1 mx-3 cursor-pointer" ]
        [ HH.text "×" ]
