-- | A proxy that hides both the Query and Message of wrapped component.
-- | Adapted from `Halogen.Component.Proxy` and `Halogen.Storybook.Proxy`.

module Internal.Proxy
  ( ProxyS
  , proxy
  ) where

import Prelude

import Data.Const (Const(..))
import Data.Coyoneda (Coyoneda, unCoyoneda)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH

data ProxyS f i a
  = Query (Coyoneda f a)

-- | A proxy that hides both the Query and Message of wrapped component.
proxy
  :: forall f i o m
   . H.Component HH.HTML f i o m
  -> H.Component HH.HTML (ProxyS (Const Void) i) i Void m
proxy = proxyEval (const (absurd <<< un Const))

proxyEval
  :: forall f g i o m
   . (forall a b. (b -> a) -> g b -> H.HalogenM i Void (child :: H.Slot f o Unit) Void m a)
  -> H.Component HH.HTML f i o m
  -> H.Component HH.HTML (ProxyS g i) i Void m
proxyEval evalQuery component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery }
  }
  where
  render :: i -> H.ComponentHTML Void (child :: H.Slot f o Unit) m
  render i = HH.slot (SProxy :: SProxy "child") unit component i (const Nothing)

  handleQuery :: forall a. ProxyS g i a -> H.HalogenM i Void (child :: H.Slot f o Unit) Void m (Maybe a)
  handleQuery (Query iq) = Just <$> unCoyoneda evalQuery iq
