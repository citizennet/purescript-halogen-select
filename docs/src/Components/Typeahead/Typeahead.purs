module Docs.Components.Typeahead where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Aff.AVar (AVAR)
import DOM (DOM)
import Data.Array (mapWithIndex, difference, filter, (:))
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Select as Select
import Select.Utils.Setters as Setters

type TypeaheadItem = String

type Effects eff = ( avar :: AVAR, dom :: DOM, console :: CONSOLE | eff )

data Query a
  = Log String a
  | HandleInputContainer (Select.Message Query TypeaheadItem) a
  | Removed TypeaheadItem a

type State =
  { items    :: Array TypeaheadItem
  , selected :: Array TypeaheadItem }

type Input = Array String
data Message = Void

type ChildSlot = Unit
type ChildQuery eff = Select.Query Query TypeaheadItem eff

component :: ∀ m e
  . MonadAff ( Effects e ) m
 => H.Component HH.HTML Query Input Message m
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: Input -> State
    initialState i = { items: i, selected: [] }

    render
      :: State
      -> H.ParentHTML Query (ChildQuery (Effects e)) ChildSlot m
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

    eval
      :: Query
      ~> H.ParentDSL State Query (ChildQuery (Effects e)) ChildSlot Message m
    eval = case _ of
      Log str a -> a <$ do
        H.liftAff $ log str

      HandleInputContainer m a -> a <$ case m of
        Select.Emit q -> eval q

        Select.Searched search -> do
          st <- H.get
          let newItems = difference st.selected <<< filterItems search $ st.items
          _ <- H.query unit $ H.action $ Select.ReplaceItems newItems
          H.liftAff $ log $ "New search: " <> search

        Select.Selected item -> do
          st <- H.get
          if length (filter ((==) item) st.items) > 0
            then H.modify _ { selected = ( item : st.selected ) }
            else H.modify _
                  { items = ( item : st.items )
                  , selected = ( item : st.selected ) }
          newSt <- H.get
          let newItems = difference newSt.items newSt.selected
          _ <- H.query unit $ H.action $ Select.ReplaceItems newItems
          H.liftAff $ log $ "New item selected: " <> item

        otherwise -> pure unit

      Removed item a -> do
        st <- H.get
        H.modify _ { selected = filter ((/=) item) st.selected }
        newSt <- H.get
        let newItems = difference newSt.items newSt.selected
        _ <- H.query unit $ H.action $ Select.ReplaceItems newItems
        pure a


----------
-- Helpers

class_ :: ∀ p i. String -> H.IProp ( "class" :: String | i ) p
class_ = HP.class_ <<< HH.ClassName

filterItems :: TypeaheadItem -> Array TypeaheadItem -> Array TypeaheadItem
filterItems str = filter (\i -> contains (Pattern str) i)

renderInputContainer :: ∀ e
  . Select.State TypeaheadItem e
 -> Select.ComponentHTML Query TypeaheadItem e
renderInputContainer state = HH.div_ [ renderInput, renderContainer ]
  where
    renderInput = HH.input $ Setters.setInputProps
      [ class_ "rounded-sm bg-white w-full flex py-2 px-3"
      , HP.placeholder "Type to search..." ]

    renderContainer =
      HH.div [ class_ "relative z-50" ]
      $ if state.visibility == Select.Off
        then []
        else [ renderItems $ renderItem `mapWithIndex` state.items ]
      where
        renderItems html =
          HH.div
          ( Setters.setContainerProps
            [ class_ "absolute bg-white shadow rounded-sm pin-t pin-l w-full" ]
          )
          [ HH.ul [ class_ "list-reset" ] html ]

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
