module Select.Primitive.Container where

import Prelude

import Control.Monad.Aff.Console (log)
import DOM.Classy.Event (preventDefault)
import DOM.Event.KeyboardEvent as KE
import DOM.Event.Types (KeyboardEvent)
import Data.Array (length, (!!))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Select.Effects (FX)

{-

The Container primitive ...

-}


-- Every primitive requires ParentQuery in order to allow the parent to embed
-- their own queries. The item and output types come from the parent so must
-- also be generic.
data Query item o a
  = ParentQuery (o Unit)            a  -- Return an embedded query to the parent
  | Highlight   Target              a  -- Change the highlighted item
  | Select      Int                 a  -- Select a particular item
  | Key         KeyboardEvent       a  -- A key has been pressed
  | Mouse       MouseState          a  -- Update mousedown state
  | Blur                            a  -- Blur event
  | Visibility  VisibilityStatus    a  -- Open or close the menu
  | SetItems    (Array (Item item)) a  -- Set the data (used by parent)

-- Possible transformations for highlight state
data Target
  = Next
  | Prev
  | Index Int

data MouseState
  = Down
  | Up

-- Possible transformations for menu state
data VisibilityStatus
  = On
  | Off
  | Toggle

type State item =
  { items            :: Array (Item item)
  , open             :: Boolean
  , highlightedIndex :: Maybe Int
  , lastIndex        :: Int
  , mouseDown        :: Boolean
  }

-- Later, should support a grid.
-- data Container item
--   = MatrixOf (Item item)
--     ListOf   (Item item)

-- Containers contain items, which can have three
-- possible states.
data Item item
  = Selected item
  | Selectable item
  | Disabled item

type Input item =
  { items :: Array (Item item) }

-- All components must allow for emitting the parent's queries back up to the parent.
-- In addition, the dropdown supports selecting items from the list.
data Message item o
  = Emit (o Unit)
  | ItemSelected item

-- The primitive handles state and transformations but defers all rendering to the parent. The
-- render function can be written using our helper functions to ensure the right events are included.
component :: ∀ item o e
   . (State item -> H.ComponentHTML (Query item o))
  -> H.Component HH.HTML (Query item o) (Input item) (Message item o) (FX e)
component render =
  H.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState i =
      { items: i.items
      , open: false
      , highlightedIndex: Nothing
      , lastIndex: length i.items - 1
      , mouseDown: false
      }
    eval :: (Query item o) ~> H.ComponentDSL (State item) (Query item o) (Message item o) (FX e)
    eval = case _ of
      ParentQuery o a -> a <$ do
        H.raise $ Emit o

      Select index a -> do
        st <- H.get
        if not st.open then pure a else a <$ case st.items !! index of
          Just (Selectable item) -> H.raise $ ItemSelected item
          _ -> H.liftAff $ log $ "Cannot select item at that index: " <> show index

      -- We can ignore the case in which we don't want anything highlighted
      -- as once the highlight becomes active, nothing but closing the menu
      -- will remove it
      Highlight target a -> do
        st <- H.get
        if not st.open then pure a else a <$ case target of

          Index i -> do
            H.modify (_ { highlightedIndex = Just i })

          Next    -> do
            st <- H.get
            case st.highlightedIndex of
              Just i | i /= st.lastIndex -> H.modify (_ { highlightedIndex = Just (i + 1) })
              otherwise -> H.modify (_ { highlightedIndex = Just 0 })

          Prev    -> do
            st <- H.get
            case st.highlightedIndex of
              Just i | i /= 0 -> H.modify (_ { highlightedIndex = Just (i - 1) })
              otherwise -> H.modify (_ { highlightedIndex = Just st.lastIndex })

      Key (ev :: KE.KeyboardEvent) a -> do
        st <- H.get
        if not st.open then pure a else a <$ case KE.code ev of

          "Enter" -> do
            H.liftEff $ preventDefault ev
            st <- H.get
            case st.highlightedIndex of
              Nothing -> pure a
              Just index -> eval (Select index a)

          "Escape" -> a <$ do
            H.modify (_ { open = false })

          "ArrowUp" -> do
            H.liftEff $ preventDefault ev
            eval $ Highlight Prev a

          "ArrowDown" -> a <$ do
            H.liftEff $ preventDefault ev
            eval $ Highlight Next a

          other -> a <$ do
            H.liftAff $ log $ show other

      Mouse ms a -> do
        st <- H.get
        if not st.open then pure a else a <$ case ms of
          Down -> do
            H.liftAff $ log $ "mouse: down"
            H.modify (_ { mouseDown = true })
          Up -> do
            H.liftAff $ log $ "mouse: up"
            H.modify (_ { mouseDown = false })

      Blur a -> do
        st <- H.get
        H.liftAff $ log $ "st.open: " <> show st.open
        H.liftAff $ log $ "st.mouseDown: " <> show st.mouseDown
        if not st.open || st.mouseDown then pure a else a <$ do
          eval $ Visibility Off a

      -- When toggling, the user will lose their highlighted index.
      Visibility status a -> a <$ case status of
        On     -> H.modify (_ { open = true })
        Off    -> H.modify (_ { open = false, highlightedIndex = Nothing })
        Toggle -> H.modify \st -> st { open = not st.open, highlightedIndex = Nothing }

      SetItems arr a -> a <$ do
        H.modify (_ { items = arr, highlightedIndex = Nothing, lastIndex = length arr - 1 })
