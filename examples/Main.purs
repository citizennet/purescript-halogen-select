module Main where

import Prelude

import Data.Array (zipWith)
import Data.Const (Const)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (for_, sequence, traverse)
import Data.Tuple (Tuple(..))
import Docs.Internal.Proxy (ProxyS, proxy)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Web.DOM.Element (getAttribute)
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.HTMLElement (HTMLElement, toElement, fromNode)
import Web.HTML.Window (document)

-- Finds all nodes labeled "data-component-id" and retrieves the associated attribute.
-- Then, mounts the right component at each node.

main :: Effect Unit
main = pure unit

{-
main :: Effect Unit
main = HA.runHalogenAff do
  elements <- awaitSelectAll
    { query: QuerySelector "div[data-component]"
    , attr: "data-component"
    }
  for_ elements \e -> runUI app e.attr e.element
-}

----------
-- Routes

{-
type Components m
  = Map.Map String (H.Component HH.HTML (ProxyS (Const Void) Unit) Unit Void m)

routes :: ∀ m. MonadAff m => Components m
routes = Map.fromFoldable
  [ Tuple "typeahead" $ proxy Component.typeahead
  , Tuple "dropdown" $ proxy Component.dropdown 
  ]

app :: ∀ m. MonadAff m => H.Component HH.HTML (Const Void) String Void m
app = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
  }
  where
  render st = Map.lookup st routes # case _ of
    Nothing -> HH.div_ []
    Just component -> HH.slot (SProxy :: SProxy "child") unit component unit absurd

----------
-- Selection Helpers

awaitSelectAll
  :: { query :: QuerySelector, attr :: String }
  -> Aff (Array { element :: HTMLElement, attr :: String })
awaitSelectAll ask@{ query } = HA.awaitLoad >>= \_ -> selectElements ask >>= pure

selectElements
  :: { query :: QuerySelector, attr :: String }
  -> Aff (Array { element :: HTMLElement, attr :: String })
selectElements { query, attr } = do
  nodeArray <- liftEffect do
    toArray =<< querySelectorAll query <<< toParentNode =<< document =<< window

  let elems :: Array HTMLElement
      elems = fromMaybe [] <<< sequence $ fromNode <$> nodeArray

  attrs <- liftEffect $ traverse (getAttribute attr <<< toElement) elems
  pure $ zipWith ({ element: _, attr: _ }) elems (fromMaybe "" <$> attrs)
-}
