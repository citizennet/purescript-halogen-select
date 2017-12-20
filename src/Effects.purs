module Select.Effects where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)

type FX e = Aff (Effects e)
type Effects e = ( dom :: DOM, console :: CONSOLE, ajax :: AJAX, avar :: AVAR | e)
