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
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import Halogen.Hooks (Hook, HookM, MemoValues, UseState, useState)
import Halogen.Hooks as Hooks

newtype UseEvent a hooks = UseEvent (UseState (Maybe a) hooks)

derive instance newtypeUseEvent :: Newtype (UseEvent a hooks) _

type EventEqFn a =
  { state :: Maybe a } -> { state :: Maybe a } -> Boolean

type EventProps slots output m a hooked =
  { capturesWith :: EventEqFn a -> (MemoValues -> hooked) -> hooked
  , subscribe :: (a -> HookM slots output m Unit) -> HookM slots output m Unit
  }

type EventApi slots output m a hooked =
  { push :: a -> HookM slots output m Unit
  , props :: EventProps slots output m a hooked
  }

useEvent
  :: forall slots output m a hooked
   . Eq a
  => Hook slots output m (UseEvent a) (EventApi slots output m a hooked)
useEvent = Hooks.wrap Hooks.do
  state /\ tState <- useState Nothing

  Hooks.pure { push: \value -> Hooks.put tState (Just value)
             , props: { capturesWith: \eqFn -> Hooks.capturesWith eqFn { state }
                      , subscribe: \cb -> do
                          state' <- Hooks.get tState
                          for_ state' cb
                      }
             }
