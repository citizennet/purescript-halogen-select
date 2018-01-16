module Select.Effects where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)

-- | The primitive Effects type, wrapped in Aff. This is used for convenience over Effects.
type FX e = Aff (Effects e)

-- | The primitive Effects type. To extend your own component with this type, you might do:
-- | main :: ∀ e. Eff (HalogenEffects (Effects e)) Unit
type Effects e = ( dom :: DOM, console :: CONSOLE, ajax :: AJAX, avar :: AVAR, now :: NOW | e)
