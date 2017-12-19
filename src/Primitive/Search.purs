module Select.Primitive.Search where

import Prelude

import Control.Monad.Aff.Console (log)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Select.Effects (FX)

{-

The Search primitive captures user input and returns it to the parent.

-}


data Query o a
  = ParentQuery (o Unit) a  -- Return an embedded query to the parent
  | TextInput String a

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
data Message o
  = Emit (o Unit)
  | NewSearch Search

component :: ∀ o e
   . (State -> H.ComponentHTML (Query o))
  -> H.Component HH.HTML (Query o) Input (Message o) (FX e)
component render =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: const Nothing
    }
  where
    eval :: Query o ~> H.ComponentDSL State (Query o) (Message o) (FX e)
    eval = case _ of
      ParentQuery o a -> a <$ do
        H.raise $ Emit o

      TextInput str a -> a <$ do
        H.liftAff $ log str
