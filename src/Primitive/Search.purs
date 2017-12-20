module Select.Primitive.Search where

import Prelude

import Control.Monad.Aff.Console (log)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Select.Effects (FX)
import Select.Dispatch (Dispatch(ParentQuery, C, S), SearchQuery(TextInput))

{-

The Search primitive captures user input and returns it to the parent.

-}

type State =
  { search   :: Maybe Search   -- If no text, Nothing
  , debounce :: Maybe Delay    -- If Nothing, don't debounce. If Just n, use N as time in ms
  }

-- Expects a delay in milliseconds
data Delay  = Delay Int
type Search = String

-- Expect a possible pre-filled state and a potential debounce value
type Input = State

-- The search serves only to notify the parent that a new search has been performed by the user.
-- If this search should cause any new data to be sent to a container, that is the responsibility
-- of the parent.
data Message item o
  = Emit (Dispatch item o Unit)
  | NewSearch Search

component :: ∀ item o e
   . (State -> H.ComponentHTML (Dispatch item o))
  -> H.Component HH.HTML (Dispatch item o) Input (Message item o) (FX e)
component render =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: const Nothing
    }
  where
    eval :: (Dispatch item o) ~> H.ComponentDSL State (Dispatch item o) (Message item o) (FX e)
    eval = case _ of
      S q a -> a <$ case q of
        TextInput str -> H.raise $ NewSearch str

      -- Boilerplate for now...
      C q a -> a <$ do
        H.raise $ Emit (C q unit)

      -- Boilerplate for now...
      ParentQuery q a -> a <$ do
        H.raise $ Emit (ParentQuery q unit)
