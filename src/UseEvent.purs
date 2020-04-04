module Example.Hooks.UseEvent
  ( useEvent
  , UseEvent
  , EventEqFn
  , EventProps
  , EventApi
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Halogen.Hooks (Hook, HookM, MemoValues, UseState, useState)
import Halogen.Hooks as Hooks

newtype UseEvent a hooks = UseEvent (UseState (Maybe a) hooks)

derive instance newtypeUseEvent :: Newtype (UseEvent a hooks) _

type EventEqFn a =
  { state :: Maybe a } -> { state :: Maybe a } -> Boolean

type EventProps slots output m a b hooked =
  { capturesWith :: EventEqFn a -> (MemoValues -> hooked) -> hooked
  , subscribe :: (a -> HookM slots output m b) -> HookM slots output m (Maybe b)
  }

type EventApi slots output m a b hooked =
  { push :: a -> HookM slots output m Unit
  , props :: EventProps slots output m a b hooked
  }

useEvent
  :: forall slots output m a b hooked
   . Eq a
  => Hook slots output m (UseEvent a) (EventApi slots output m a b hooked)
useEvent = Hooks.wrap Hooks.do
  state /\ tState <- useState Nothing

  Hooks.pure { push: \value -> Hooks.put tState (Just value)
             , props: { capturesWith: \eqFn -> Hooks.capturesWith eqFn { state }
                      , subscribe: \cb -> do
                          state' <- Hooks.get tState
                          for state' cb
                      }
             }
