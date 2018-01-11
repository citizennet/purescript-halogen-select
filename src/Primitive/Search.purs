module Select.Primitive.Search where

import Prelude

import Control.Monad.Aff (Error, Fiber, delay, error, forkAff, killFiber)
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, putVar, takeVar)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds)
import Halogen (Component, ComponentDSL, ComponentHTML, component, get, liftAff, modify) as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.HalogenM (fork, raise) as H
import Select.Dispatch (Dispatch(..), SearchQuery(..), SearchState, SearchInput, updateStore, getState, State)
import Select.Effects (FX, Effects)
import Control.Comonad
import Data.Tuple
import Control.Comonad.Store

{-

The Search primitive captures user input and returns it to the parent.

-}

data Message item o e
  = Emit (Dispatch item o e Unit)
  | NewSearch String

component :: ∀ item o e. H.Component HH.HTML (Dispatch item o e) (SearchInput item o e) (Message item o e) (FX e)
component =
  H.component
    { initialState
    , render: extract
    , eval
    , receiver: const Nothing -- HE.input (Search <<< SearchReceiver)
    }
  where
    initialState :: SearchInput item o e -> State (SearchState e) item o e
    initialState i = store i.render
      { search: fromMaybe "" i.search
      , ms: i.debounceTime
      , debouncer: Nothing
      }

    eval :: (Dispatch item o e) ~> H.ComponentDSL (State (SearchState e) item o e) (Dispatch item o e) (Message item o e) (FX e)
    eval = case _ of
      ParentQuery q a -> a <$ do
        H.raise $ Emit (ParentQuery q unit)

      Container q a -> a <$ do
        H.raise $ Emit (Container q unit)

      Search q a -> case q of
        TextInput str -> a <$ do
          (Tuple _ st) <- getState
          H.modify $ seeks _ { search = str }

          case st.debouncer of
            Nothing -> unit <$ do
              var <- H.liftAff makeEmptyVar
              fiber <- H.liftAff $ forkAff do
                  delay st.ms
                  putVar unit var

              -- This computation will fork and run later. When the var is finally filled,
              -- it will run the effect.
              _ <- H.fork $ do
                _ <- H.liftAff $ takeVar var
                H.modify $ seeks _ { debouncer = Nothing }

                (Tuple _ st) <- getState
                H.raise $ NewSearch st.search

              H.modify $ seeks \st -> st { debouncer = Just { var, fiber } }

            Just debouncer -> do
              let var = debouncer.var
              _ <- H.liftAff $ killFiber (error "Time's up!") debouncer.fiber

              fiber <- H.liftAff $ forkAff do
                  delay st.ms
                  putVar unit var

              H.modify $ seeks _ { debouncer = Just { var, fiber } }


        -- Only update `render`. To send a new search to the field, use a query from
        -- the parent to set the text.
        SearchReceiver i -> a <$ do
           H.modify $ updateStore i.render id
