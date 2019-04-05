module Docs.Components.Typeahead where

import Prelude

import Data.Array (elemIndex, mapWithIndex, difference, filter, (:))
import Data.Foldable (for_, length)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.String (Pattern(..), contains)
import Data.Symbol (SProxy(..))
import Docs.CSS as CSS
import Docs.Components.Dropdown as Dropdown
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select as Select
import Select.Setters as Setters

{-
  You can make complex selection interfaces like typeaheads the same way you can
  make simpler ones like dropdowns. It just comes down to how much of Select you
  want to extend.

  In this case, we'll extend the component so that it can manage a list of selections,
  where no selected item should be displayed in the list of available options. We'll
  need to extend the component state with the list of selections; we'll need to extend
  the query algebra with the ability to click to remove items; and we'll go ahead and
  add a new message that lets a parent component know when an item was removed.

  Plus, we'll get fancy: we'll have a dropdown running *inside* Select, using our
  previous work.
-}

-----
-- Component

-- Just like the dropdown, all we have to do to export a full component is just 
-- specialize the types and supply a render function, query handler, and message 
-- handler.
component :: forall m. MonadAff m => H.Component HH.HTML Query Input Message m
component = Select.component render handleQuery handleMessage


-----
-- State

-- Select doesn't maintain any selections on its own, but we can extend it to 
-- hold a list of selected items by adding new contents to state. We'll need
-- three lists: a list of all items, a list of non-selected items, and a list
-- of selected items.
type State = Select.State String ExtraState

type ExtraState =
  ( selectedItems :: Array String
  , visibleItems :: Array String
  )

-- Our input type will also need these items
type Input = Select.Input String ExtraState


-----
-- Child Components

-- We're going to embed a dropdown in the component, so we'll extend Select with
-- new child slots.
type ChildSlots = 
  ( dropdown :: H.Slot Dropdown.Query Dropdown.Message Unit )

_dropdown = SProxy :: SProxy "dropdown"


-----
-- Messages

-- We'll add a new message to Select to cover the case when an item is removed. The
-- parent might wish to display a confirmation of the last-removed item or use it
-- otherwise.
type Message = Select.Message String ExtraMessage

data ExtraMessage 
  = ItemRemoved String 

-- We also need to take action when an item is selected in Select or a debounced
-- search has completed. We'll do that by handling the message within Select.
handleMessage 
  :: forall m
   . MonadAff m
  => Message 
  -> H.HalogenM State Action ChildSlots Message m Unit
handleMessage = case _ of
  Select.Selected item -> do
    st <- H.get
    let newSelections = item : st.selectedItems
    H.modify_ _ 
      { selectedItems = newSelections
      , visibleItems = difference newSelections st.items
      , search = ""
      }
  
  Select.Searched str -> do
    st <- H.get
    let 
      visible = difference st.selectedItems $ filter (contains (Pattern str)) st.items
      index = elemIndex str visible
    H.modify_ _ { visibleItems = visible }
    for_ index \ix -> do
      Select.handleAction handleQuery handleMessage $ Select.Highlight $ Select.Index ix
  
  _ -> 
    pure unit



-----
-- Query

-- We introduced some new behavior for selecting items via the `handleMessage` function
-- but we also need to support removal and we need to handle messages output by our
-- further child component, the dropdown.
type Query = Select.Query String ExtraQuery ChildSlots

-- We'll provide a synonym for the action type, too.
type Action = Select.Action String ExtraQuery ChildSlots

data ExtraQuery a
  = Remove String a
  | HandleDropdown Dropdown.Message a

handleQuery 
  :: forall m a
   . MonadAff m
  => ExtraQuery a
  -> H.HalogenM State Action ChildSlots Message m (Maybe a)
handleQuery = case _ of  
  Remove item a -> Just a <$ do
    st <- H.get
    let newSelections = filter (_ /= item) st.selectedItems
    H.modify_ _
      { selectedItems = newSelections
      , visibleItems = difference newSelections st.items
      }
    H.raise $ Select.Raised $ ItemRemoved item
  
  -- We can handle our child component
  HandleDropdown msg a -> Just a <$ case msg of
    Select.Selected item -> do
      st <- H.get
      let index = elemIndex item st.items
      for_ index \ix -> 
        -- Remember that we're handling this from the typeahead; calling
        -- the below will cause the *typeahead* to select the item, not the
        -- dropdown.
        Select.handleAction handleQuery handleMessage 
          $ Select.Select (Select.Index ix) Nothing

    _ -> 
      pure unit

-----
-- Render Function

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render state = 
  HH.div_ 
    [ renderSelections, renderInput, renderContainer ]
  where
  renderSelections :: forall props. HH.HTML props Action
  renderSelections = case length state.selectedItems of
    0 -> 
      HH.div_ []
    _ ->
      HH.div
        [ class_ "bg-white rounded-sm w-full border-b border-grey-lighter" ]
        [ HH.ul
          [ class_ "list-reset" ]
          (renderSelectedItem <$> state.selectedItems)
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
        [ HE.onClick \_ -> 
	    -- nicer with variants. worth it?
            Just $ Select.RunQuery $ Select.Embed $ Remove item unit
        , class_ "absolute pin-t pin-b pin-r p-1 mx-3 cursor-pointer" 
        ]
        [ HH.text "×" ]
  
  renderInput = HH.input $ Setters.setInputProps
    [ HP.classes CSS.input
    , HP.placeholder "Type to search..." 
    ]

  renderContainer =
    HH.div 
      [ class_ "relative z-50" ]
      if state.visibility == Select.Off
        then []
        else [ renderItems $ renderItem `mapWithIndex` state.visibleItems ]
    where
    -- here we can render a further child component, the dropdown, which is *also*
    -- a select component.
    renderChild = HH.slot _dropdown unit Dropdown.component input handleChild
      where
      -- better with variants; worth it?
      handleChild msg = Just (Select.RunQuery (Select.Embed (HandleDropdown msg unit)))

      input = 
        { inputType: Select.Toggle
	, items: [ "Choice A", "Choice B" ]
	, search: Nothing
	, debounceTime: Nothing
	, selection: Nothing
	}

    renderItems html =
      HH.div
        ( Setters.setContainerProps
          [ class_ "absolute bg-white shadow rounded-sm pin-t pin-l w-full" ]
        )
        [ HH.ul 
            [ class_ "list-reset" ] 
            html 
        , renderChild
        ]

    renderItem index item =
      HH.li (Setters.setItemProps index [ class_ (base <> extra) ]) [ HH.text item ]
      where
      base = "px-4 py-1 text-grey-darkest"
      extra = " bg-grey-lighter" # guard (state.highlightedIndex == Just index) 


-----
-- Helpers

class_ :: ∀ p i. String -> HH.IProp ( "class" :: String | i ) p
class_ = HP.class_ <<< HH.ClassName

