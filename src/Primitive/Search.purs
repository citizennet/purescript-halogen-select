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
import Select.Dispatch (Dispatch(ParentQuery, Container, Search), SearchQuery(TextInput))
import Select.Effects (FX, Effects)

{-

The Search primitive captures user input and returns it to the parent.

-}

type State e =
  { search    :: String
  , ms        :: Milliseconds
  , debouncer :: Maybe (Debouncer e)
  }

type Debouncer e =
  { var :: AVar String
  , fiber :: Fiber (Effects e) Unit }


type Input =
  { search :: Maybe String
  , debounceTime :: Milliseconds }

-- The search serves only to notify the parent that a new search has been performed by the user.
-- If this search should cause any new data to be sent to a container, that is the responsibility
-- of the parent.
data Message item o
  = Emit (Dispatch item o Unit)
  | NewSearch String

component :: ∀ item o e
   . (State e -> H.ComponentHTML (Dispatch item o))
  -> H.Component HH.HTML (Dispatch item o) Input (Message item o) (FX e)
component render =
  H.component
    { initialState: \i -> { search: fromMaybe "" i.search, ms: i.debounceTime, debouncer: Nothing }
    , render
    , eval
    , receiver: const Nothing
    }
  where
    eval :: (Dispatch item o) ~> H.ComponentDSL (State e) (Dispatch item o) (Message item o) (FX e)
    eval = case _ of
      -- The dispatch type matches this primitive -- the Search primitive
      Search q a -> case q of
        TextInput str -> do
          st  <- H.get
          H.modify _ { search = str }

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
                H.modify _ { debouncer = Nothing }

                -- Run the effect, making sure to get from the state.
                st <- H.get
                H.raise $ NewSearch st.search

              -- In the meantime -- while the other fork is running -- create the new debouncer
              -- in state so it can continue to be accessed.
              H.modify \st -> st { debouncer = Just { var, fiber } }


            Just debouncer -> do
              let var = debouncer.var
              _ <- H.liftAff $ killFiber (error "Time's up!") debouncer.fiber

              (fiber :: Fiber (Effects e) Unit) <- H.liftAff $ forkAff do
                  delay st.ms
                  putVar str var

              H.modify _ { debouncer = Just { var, fiber } }


          pure a
          -- H.raise $ NewSearch str

      -- Boilerplate for now...raise container queries back to the parent
      Container q a -> a <$ do
        H.raise $ Emit (Container q unit)

      -- Boilerplate for now...raise parent queries back to the parent.
      ParentQuery q a -> a <$ do
        H.raise $ Emit (ParentQuery q unit)
