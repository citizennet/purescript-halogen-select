-- | All primitives use the `Store` comonad as their component state. They also
-- | define their own state as is typical for Halogen Components, which is in turn
-- | embedded in `Store`.
-- |
-- | Note: Unless you are modifying or defining a new primitive, you will not need to use this type
-- | and can rely on the usual state definitions in each primitive's module.
-- |
-- |
-- | **Why use `Store`?**
-- |
-- | Halogen components only update if new values come in via their `receiver` field using their `Input`
-- | type, or if they are destroyed and re-initialized in a new slot. Most values can be updated this
-- | way, but not render functions.
-- |
-- | In this library, we do not make any rendering decisions and leave it entirely up to you to decide
-- | how your selection component should appear. Your render functions have access to the parent state
-- | in which you mounted the primitive. This means you can provide arbitrary data to the primitive by
-- | storing it in the parent state and accessing it in the renderer.
-- |
-- | Therefore, it is necessary that your render function updates on parent re-render just like usual
-- | `Input` values. Using `Store` allows us to extract and use the render function in a primitive.

module Select.Primitives.State where

import Prelude (pure, (<<<), (=<<))

import Control.Comonad.Store (Store, runStore, seeks, store)
import Data.Tuple (Tuple(..))
import Control.Monad.State (class MonadState)
import Halogen as H

-- | All primitives use the `Store` type as their component state. Any additional
-- | data, like a traditional Halogen State record, will also be provided.
-- |
-- | - `s`: The state type defined for the primitive
-- | - `q`: The query type defined for the primitive
type State s q = Store s (H.ComponentHTML q)

-- | A helper to get and unpack the primitive state type from the Store type. When used with pattern matching,
-- | you can access state with:
-- |
-- | ```purescript
-- | (Tuple renderFunction state) <- getState
-- | ```
getState :: ∀ m s a. MonadState (Store s a) m => m (Tuple (s -> a) s)
getState = pure <<< runStore =<< H.get

-- | A helper for wholly updating the `State` (`Store`) of a primitive.
-- |
-- | Used when the `render` function needs to be updated; this is typically in the
-- | query that handles `Input` updates.
-- |
-- | Note: Use `seeks` if only the primitive's internal state needs to be updated (not the entire Store).
updateStore :: ∀ state html. (state -> html) -> (state -> state) -> Store state html -> Store state html
updateStore r f = (\(Tuple _ s) -> store r s) <<< runStore <<< seeks f
