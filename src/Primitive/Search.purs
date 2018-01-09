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
import Select.Dispatch (Dispatch(..), SearchQuery(..), SearchState, SearchInput, updateState, updateStore)
import Select.Effects (FX, Effects)
import Control.Comonad
import Data.Tuple
import Control.Comonad.Store

{-

The Search primitive captures user input and returns it to the parent.

-}

type State item o e = Store (SearchState e) (H.ComponentHTML (Dispatch item o e))

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
    initialState :: SearchInput item o e -> State item o e
    initialState i = store i.render
      { search: fromMaybe "" i.search
      , ms: i.debounceTime
      , debouncer: Nothing
      }

    eval :: (Dispatch item o e) ~> H.ComponentDSL (State item o e) (Dispatch item o e) (Message item o e) (FX e)
    eval = case _ of
      -- The dispatch type matches this primitive -- the Search primitive
      Search q a -> case q of
        TextInput str -> do
          (Tuple _ st) <- pure <<< runStore =<< H.get
          H.modify $ updateState _ { search = str }

          case st.debouncer of
            Nothing -> unit <$ do
              (var :: AVar String) <- H.liftAff makeEmptyVar

              (fiber :: Fiber (Effects e) Unit) <- H.liftAff $ forkAff do
                  delay st.ms
                  putVar str var

              -- This computation will fork and run later. When the var is finally filled,
              -- it will run the effect.
              (x :: Error -> (FX e) Unit) <- H.fork $ do
                -- This won't happen until there is something in the var to take
                _ <- H.liftAff $ takeVar var

                -- Reset the debouncer
                H.modify $ updateState _ { debouncer = Nothing }

                -- Run the effect, making sure to get from the state.
                (Tuple _ st) <- pure <<< runStore =<< H.get
                H.raise $ NewSearch st.search

              -- In the meantime -- while the other fork is running -- create the new debouncer
              -- in state so it can continue to be accessed.
              H.modify $ updateState \st -> st { debouncer = Just { var, fiber } }


            Just debouncer -> do
              let var = debouncer.var
              _ <- H.liftAff $ killFiber (error "Time's up!") debouncer.fiber

              (fiber :: Fiber (Effects e) Unit) <- H.liftAff $ forkAff do
                  delay st.ms
                  putVar str var

              H.modify $ updateState _ { debouncer = Just { var, fiber } }

          pure a

        -- Only update `render`. To send a new search to the field, use a query from
        -- the parent to set the text.
        SearchReceiver i -> a <$ do
           H.modify $ updateStore i.render id

      -- Boilerplate for now...raise container queries back to the parent
      Container q a -> a <$ do
        H.raise $ Emit (Container q unit)

      -- Boilerplate for now...raise parent queries back to the parent.
      ParentQuery q a -> a <$ do
        H.raise $ Emit (ParentQuery q unit)
