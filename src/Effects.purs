module Select.Effects where

import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)

-- | The effect rows used in all primitives. To extend this type in a parent component, you
-- | can add your own effects:
-- |
-- | ```purescript
-- | type MyEffects e = ( effect :: EFFECT | Effects e )
-- | ```
-- |
-- | Note: When using the Search primitive, make sure you provide effects extended by Effects, rather than your own
-- | effects directly. Using the above MyEffects example:
-- |
-- | ```purescript
-- | type ChildQuery e = SearchQuery MyQuery MyItem (MyEffects e)
-- | ```
type Effects eff = ( dom :: DOM, console :: CONSOLE, ajax :: AJAX, avar :: AVAR, ref :: REF, exception :: EXCEPTION | eff )
