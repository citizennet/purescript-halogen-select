-- | A proxy that hides both the Query and Message of wrapped component.
-- | Adapted from `Halogen.Component.Proxy` and `Halogen.Storybook.Proxy`.

module Docs.Internal.Proxy
  ( ProxyS
  , proxy
  ) where

import Prelude

import Data.Const (Const(..))
import Data.Coyoneda (Coyoneda, unCoyoneda)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
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
   . (forall a b. (b -> a) -> g b -> H.ParentDSL i (ProxyS g i) f Unit Void m a)
  -> H.Component HH.HTML f i o m
  -> H.Component HH.HTML (ProxyS g i) i Void m
proxyEval evalQuery component =
  H.parentComponent
    { initialState: id
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: i -> H.ParentHTML (ProxyS g i) f Unit m
    render i = HH.slot unit component i (const Nothing)

    eval :: ProxyS g i ~> H.ParentDSL i (ProxyS g i) f Unit Void m
    eval (Query iq) = unCoyoneda evalQuery iq
