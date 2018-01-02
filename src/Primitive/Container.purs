module Select.Primitive.Container where

import Prelude

import Control.Monad.Aff.Console (log)
import DOM.Classy.Event (preventDefault)
import DOM.Event.KeyboardEvent as KE

import Data.Array (length, (!!))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Select.Dispatch (ContainerQuery(..), Dispatch(..), Item(..), MouseState(..), Target(..), VisibilityStatus(..))
import Select.Effects (FX)

{-

The Container primitive ...

-}

type State item =
  { items            :: Array (Item item)
  , open             :: Boolean
  , highlightedIndex :: Maybe Int
  , lastIndex        :: Int
  , mouseDown        :: Boolean
  }

type Input item =
  { items :: Array (Item item) }

-- All components must allow for emitting the parent's queries back up to the parent.
-- In addition, the dropdown supports selecting items from the list.
data Message item o
  = Emit (Dispatch item o Unit)
  | ItemSelected item

-- The primitive handles state and transformations but defers all rendering to the parent. The
-- render function can be written using our helper functions to ensure the right events are included.
component :: ∀ item o e
   . (State item -> H.ComponentHTML (Dispatch item o))
  -> H.Component HH.HTML (Dispatch item o) (Input item) (Message item o) (FX e)
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
    eval :: (Dispatch item o) ~> H.ComponentDSL (State item) (Dispatch item o) (Message item o) (FX e)
    eval = case _ of
      -- Boilerplate for now...emits a ParentQuery back up
      ParentQuery q a -> a <$ do
        H.raise $ Emit (ParentQuery q unit)

      -- Boilerplate for now...emits a SearchQuery back up
      S q a -> a <$ do
        H.raise $ Emit (S q unit)

      C q a -> case q of
        Select index -> do
          st <- H.get
          if not st.open then pure a else a <$ case st.items !! index of
            Just (Selectable item) -> H.raise $ ItemSelected item
            _ -> H.liftAff $ log $ "Cannot select item at that index: " <> show index

        -- We can ignore the case in which we don't want anything highlighted
        -- as once the highlight becomes active, nothing but closing the menu
        -- will remove it
        Highlight target -> do
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

        Key (ev :: KE.KeyboardEvent) -> do
          st <- H.get
          if not st.open then pure a else case KE.code ev of

            "Enter" -> do
              H.liftEff $ preventDefault ev
              st <- H.get
              case st.highlightedIndex of
                Nothing -> pure a
                Just index -> eval $ C (Select index) a

            "Escape" -> a <$ do
              H.modify (_ { open = false })

            "ArrowUp" -> a <$ do
              H.liftEff $ preventDefault ev
              eval $ C (Highlight Prev) a

            "ArrowDown" -> a <$ do
              H.liftEff $ preventDefault ev
              eval $ C (Highlight Next) a

            other -> pure a

        Mouse ms -> do
          st <- H.get
          if not st.open then pure a else a <$ case ms of
            Down -> do
              H.modify (_ { mouseDown = true })
            Up -> do
              H.modify (_ { mouseDown = false })

        Blur -> do
          st <- H.get
          if not st.open || st.mouseDown then pure a else a <$ do
            -- You're forced to wrap in Dispatch
            eval $ C (Visibility Off) unit

        -- When toggling, the user will lose their highlighted index.
        Visibility status -> a <$ case status of
          On     -> H.modify (_ { open = true })
          Off    -> H.modify (_ { open = false, highlightedIndex = Nothing })
          Toggle -> H.modify \st -> st { open = not st.open, highlightedIndex = Nothing }

        SetItems arr -> a <$ do
          H.modify (_ { items = arr, highlightedIndex = Nothing, lastIndex = length arr - 1 })
