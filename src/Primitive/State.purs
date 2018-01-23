module Select.Primitive.State where

import Prelude (pure, (<<<), (=<<))

import Control.Comonad.Store (Store, runStore, seeks, store)
import Data.Tuple (Tuple(..))
import Control.Monad.State (class MonadState)
import Halogen as H

-- | All primitives use the `Store` type as their component state. Any additional
-- | data, like a traditional Halogen State record, will usually be provided.
-- |
-- | Note: It's necessary to use a `Store` comonad as the state type for primitives
-- | because they receive their render function as input. The render function cannot
-- | be stored in the State type synonym due to cycles, and as your component render
-- | function can only take `State` as its input, you need the render function available
-- | via the comonad, StoreT.
-- |
-- | - `s`: The state type defined for the primitive
-- |
-- | - `q`: The query type defined for the primitive
type State s q = Store s (H.ComponentHTML q)

-- | Helper to get and unpack the primitive state type from the Store type. When used with pattern matching,
-- | you can access state with:
-- |
-- | ```purescript
-- | (Tuple renderFunction state) <- getState
-- | ```
getState :: ∀ m s a. MonadState (Store s a) m => m (Tuple (s -> a) s)
getState = pure <<< runStore =<< H.get

-- | Helper for wholly updating the `State` (`Store`) of a primitive.
-- |
-- | Used when the `render` function needs to be updated.
-- |
-- | Note: Use `seeks` if only the primitive's internal state needs to be updated (not the entire Store).
updateStore :: ∀ state html. (state -> html) -> (state -> state) -> Store state html -> Store state html
updateStore r f = (\(Tuple _ s) -> store r s) <<< runStore <<< seeks f
