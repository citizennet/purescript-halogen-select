module Select.Dropdown where

import Prelude
import Select.Utils (augmentHTML)

import Data.Array ((:))
import DOM.Event.Types (MouseEvent)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE


{-
The dropdown is the simplest select component. It consists of a toggle and a list
of items the user can select.
-}

-- All components require the ParentQuery query in order to allow the parent
-- to send in queries in HTML. The parent is responsible for setting data in
-- this component, so it's necessary to include the item type.
data Query item o a
  = ParentQuery (o Unit) a
  | Select item a
  | Toggle a
  | SetItems (Array item) a

-- This record should be considered read-only by the child except for the `open`
-- field. The parent will consistently write the `items` field.
type State item o =
  { render :: Render item o
  , items :: Array item
  , open :: Boolean }

-- The configuration required to initialize the component. The user must supply
-- the necessary render functions and a list of their data.
type Input item o =
  { render :: Render item o
  , items :: Array item }

-- The render record contains the two functions used by the component for display.
-- We expect the user to use our corresponding helper functions to construct these.
type Render item o =
  { toggle :: H.HTML Void (Query item o)
  , items  :: Array item -> H.HTML Void (Query item o) }

-- All components must allow for emitting the parent's queries back up to the parent.
-- In addition, the dropdown supports selecting items from the list.
data Message item o
  = Emit (o Unit)
  | Selected item

component :: ∀ item o. H.Component HH.HTML (Query item o) (Input item o) (Message item o) _
component =
  H.component
    { initialState: \i -> { render: i.render, items: i.items, open: false }
    , render
    , eval
    , receiver: const Nothing
    }
  where
    -- The dropdown component exists purely to trigger the parent's render functions after
    -- they've been augmented by our helpers. For that reason, we simply need to list out
    -- their items.
    render :: (State item o) -> H.ComponentHTML (Query item o)
    render st =
      if not st.open
      then HH.div_ [ st.render.toggle ]
      else HH.div_ [ st.render.toggle, st.render.items st.items ]

    eval :: (Query item o) ~> H.ComponentDSL (State item o) (Query item o) (Message item o) _
    eval = case _ of
      ParentQuery o a -> a <$ do
        H.raise $ Emit o

      Select item a -> a <$ do
        H.raise (Selected item)

      Toggle a -> a <$ do
        H.modify \st -> st { open = not st.open }

      SetItems arr a -> a <$ do
        H.modify \st -> st { items = arr }


--
-- RENDER HELPERS

-- A convenience for the parent to ensure they embed their queries
-- properly.
embedQuery :: ∀ item t f. (Unit -> t Unit) -> f -> Query item t f
embedQuery = ParentQuery <<< H.action

-- Two render functions are required:
-- - Toggle: some clickable region to toggle the menu status
-- - Item: the user's data rendered into the menu list

getToggleProps :: ∀ item t f
  . Array (H.IProp ( onClick :: MouseEvent | t ) (Query item f))
 -> Array (H.IProp ( onClick :: MouseEvent | t ) (Query item f))
getToggleProps = augmentHTML [ HE.onClick $ HE.input_ Toggle ]

getItemProps :: ∀ item t f
  . item
 -> Array (H.IProp ( onClick :: MouseEvent | t ) (Query item f))
 -> Array (H.IProp ( onClick :: MouseEvent | t ) (Query item f))
getItemProps item = augmentHTML [ HE.onClick $ HE.input_ $ Select item ]
