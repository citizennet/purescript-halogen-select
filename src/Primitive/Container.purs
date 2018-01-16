module Select.Primitive.Container where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (seeks, store)
import Control.Monad.Aff.Console (log)
import Data.Array (length, (!!))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import DOM.Classy.Event (preventDefault)
import DOM.Event.KeyboardEvent as KE
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Select.Dispatch (ContainerQuery(..), Dispatch(..), MouseState(..), Target(..), VisibilityStatus(..), ContainerState, ContainerInput, updateStore, getState, State)
import Select.Effects (FX)

{-

The Container primitive ...

-}

-- | The Container sends the parent messages in two instances:
-- | Emit: an embedded query has been triggered, and you must decide how to handle it; typically via evaluating
-- | in the parent or re-routing the query to another primitive.
-- | ItemSelected: an item has been selected from the container.
data Message item o e
  = Emit (Dispatch item o e Unit)
  | ItemSelected item

-- | The primitive handles state and transformations but defers all rendering to the parent. The
-- | render function can be written using our helper functions to ensure the right events are included. See the `Dispatch`
-- | module for more information.
component :: ∀ item o e. H.Component HH.HTML (Dispatch item o e) (ContainerInput item o e) (Message item o e) (FX e)
component =
  H.component
    { initialState
    , render: extract
    , eval
    , receiver: HE.input (Container <<< ContainerReceiver)
    }
  where
    initialState :: ContainerInput item o e -> State (ContainerState item) item o e
    initialState i = store i.render
      { items: i.items
      , open: false
      , highlightedIndex: Nothing
      , lastIndex: length i.items - 1
      , mouseDown: false
      }

    eval :: (Dispatch item o e) ~> H.ComponentDSL (State (ContainerState item) item o e) (Dispatch item o e) (Message item o e) (FX e)
    eval = case _ of
      ParentQuery q a -> a <$ do
        H.raise $ Emit (ParentQuery q unit)

      Search q a -> a <$ do
        H.raise $ Emit (Search q unit)

      Container q a -> case q of
        Select index -> do
          (Tuple _ st) <- getState
          if not st.open
            then pure a
            else a <$ case st.items !! index of
              Just item -> H.raise $ ItemSelected item
              _ -> H.liftAff $ log $ "Index " <> show index <> " is out of bounds."

        -- We can ignore the case in which we don't want anything highlighted
        -- as once the highlight becomes active, nothing but closing the menu
        -- will remove it
        Highlight target -> do
          (Tuple _ st) <- getState
          if not st.open then pure a else a <$ case target of
            Index i -> do
              H.modify $ seeks (_ { highlightedIndex = Just i } )
            Next    -> do
              case st.highlightedIndex of
                Just i | i /= st.lastIndex -> H.modify $ seeks (_ { highlightedIndex = Just (i + 1) })
                otherwise -> H.modify $ seeks (_ { highlightedIndex = Just 0 })
            Prev    -> do
              case st.highlightedIndex of
                Just i | i /= 0 -> H.modify $ seeks (_ { highlightedIndex = Just (i - 1) })
                otherwise -> H.modify $ seeks (_ { highlightedIndex = Just st.lastIndex })

        Key (ev :: KE.KeyboardEvent) -> do
          (Tuple _ st) <- getState
          if not st.open then pure a else case KE.code ev of
            "Enter" -> do
              H.liftEff $ preventDefault ev
              case st.highlightedIndex of
                Nothing -> pure a
                Just index -> eval $ Container (Select index) a
            "Escape" -> a <$ do
              H.modify $ seeks (_ { open = false })
            "ArrowUp" -> a <$ do
              H.liftEff $ preventDefault ev
              eval $ Container (Highlight Prev) a
            "ArrowDown" -> a <$ do
              H.liftEff $ preventDefault ev
              eval $ Container (Highlight Next) a
            other -> pure a

        Mouse ms -> do
          (Tuple _ st) <- getState
          if not st.open
            then pure a
            else a <$ case ms of
              Down -> H.modify $ seeks (_ { mouseDown = true })
              Up   -> H.modify $ seeks (_ { mouseDown = false })

        Blur -> do
          (Tuple _ st) <- getState
          if not st.open || st.mouseDown
            then pure a
            else a <$ (eval $ Container (Visibility Off) unit)

        -- When toggling, the user will lose their highlighted index.
        Visibility status -> a <$ case status of
          On     -> H.modify $ seeks (_ { open = true })
          Off    -> H.modify $ seeks (_ { open = false, highlightedIndex = Nothing })
          Toggle -> H.modify $ seeks (\st -> st { open = not st.open, highlightedIndex = Nothing })

        ContainerReceiver i -> a <$ do
          H.modify
            $ updateStore i.render
            $ (_ { items = i.items, highlightedIndex = Nothing, lastIndex = length i.items - 1 })

