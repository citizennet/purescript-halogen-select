module Select.Dropdown where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.Classy.Event (preventDefault)
import DOM.Event.KeyboardEvent as KE
import DOM.Event.Types (KeyboardEvent, MouseEvent)
import Data.Array (length, (!!))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Network.HTTP.Affjax (AJAX)
import Select.Utils (augmentHTML)


{-

The dropdown is the simplest select component. It consists of a toggle and a list
of items the user can select.

-}

type FX e = Aff (Effects e)
type Effects e = ( dom :: DOM, console :: CONSOLE, ajax :: AJAX | e)

-- All components require the ParentQuery query in order to allow the parent
-- to send in queries in HTML. The parent is responsible for setting data in
-- this component, so it's necessary to include the item type.
data Query item o a
  = ParentQuery (o Unit) a   -- Return an embedded query to the parent
  | Highlight Target a       -- Change the highlighted item
  | Select Int a             -- Select a particular item
  | Key KeyboardEvent a      -- A key has been pressed
  | Toggle a                 -- Open or close the menu
  | SetItems (Array item) a  -- Set the data (used by parent)


data Target
  = Next
  | Prev
  | Index Int

-- This record should be considered read-only by the child except for the `open`
-- field. The parent will consistently write the `items` field.
type State item =
  { items :: Array item
  , open :: Boolean
  , highlightedIndex :: Maybe Int
  , lastIndex :: Int }


type Input item =
  { items :: Array item }

-- All components must allow for emitting the parent's queries back up to the parent.
-- In addition, the dropdown supports selecting items from the list.
data Message item o
  = Emit (o Unit)
  | Selected item

-- The component is responsible for behaviors but defers the render completely to the parent.
-- That render function can be written with the help of our `getProps` helpers, or you can
-- write it yourself completely.
component :: ∀ item o e
   . (State item -> H.ComponentHTML (Query item o))
  -> H.Component HH.HTML (Query item o) (Input item) (Message item o) (FX e)
component render =
  H.component
    { initialState: \i -> { items: i.items, open: false, highlightedIndex: Nothing, lastIndex: (length i.items) - 1 }
    , render
    , eval
    , receiver: const Nothing
    }
  where
    eval :: (Query item o) ~> H.ComponentDSL (State item) (Query item o) (Message item o) (FX e)
    eval = case _ of
      ParentQuery o a -> a <$ do
        H.raise $ Emit o

      Select index a -> a <$ do
        st <- H.get
        case st.items !! index of
          Nothing -> H.liftAff $ log $ "No item at that index: " <> show index
          Just item -> H.raise $ Selected item

      -- We can ignore the case in which we don't want anything highlighted
      -- as once the highlight becomes active, nothing but closing the menu
      -- will remove it
      Highlight target a -> a <$ case target of
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

      Key (ev :: KE.KeyboardEvent) a → do
        case KE.code ev of
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
            eval $ Highlight Next a

          "ArrowDown" -> a <$ do
            H.liftEff $ preventDefault ev
            eval $ Highlight Prev a

          other → a <$ do
            H.liftAff $ log $ show other

      -- When toggling, the user will lose their highlighted index.
      Toggle a -> a <$ do
        H.modify \st -> st { open = not st.open, highlightedIndex = Nothing }

      SetItems arr a -> a <$ do
        H.modify (_ { items = arr, highlightedIndex = Nothing })


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
  . Int
 -> Array (H.IProp ( onClick :: MouseEvent | t ) (Query item f))
 -> Array (H.IProp ( onClick :: MouseEvent | t ) (Query item f))
getItemProps index = augmentHTML [ HE.onClick $ HE.input_ $ Select index ]
