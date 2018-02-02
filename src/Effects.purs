module Select.Effects where

import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)

-- | The primitive Effects type. To extend your own component with this type, you might do:
-- |
-- | ```purescript
-- | main :: ∀ e. Eff (HalogenEffects (Effects e)) Unit
-- | ```
type Effects eff = ( dom :: DOM, console :: CONSOLE, ajax :: AJAX, avar :: AVAR, ref :: REF, exception :: EXCEPTION | eff )
