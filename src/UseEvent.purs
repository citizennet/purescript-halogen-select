module Example.Hooks.UseEvent
  ( useEvent
  , UseEvent
  , EventProps
  , EventApi
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Halogen.Hooks (Hook, HookM, UseState, useState)
import Halogen.Hooks as Hooks

newtype UseEvent a hooks = UseEvent (UseState (Maybe a) hooks)

derive instance newtypeUseEvent :: Newtype (UseEvent a hooks) _

type EventProps slots output m a b =
  { deps :: { state :: Maybe a }
  , subscribe :: (a -> HookM slots output m b) -> HookM slots output m (Maybe b)
  }

type EventApi slots output m a b =
  { push :: a -> HookM slots output m Unit
  , props :: EventProps slots output m a b
  }

useEvent
  :: forall slots output m a b
   . Eq a
  => Hook slots output m (UseEvent a) (EventApi slots output m a b)
useEvent = Hooks.wrap Hooks.do
  state /\ tState <- useState Nothing

  Hooks.pure { push: \value -> Hooks.put tState (Just value)
             , props: { deps: { state }
                      , subscribe: \cb -> do
                          state' <- Hooks.get tState
                          for state' cb
                      }
             }
