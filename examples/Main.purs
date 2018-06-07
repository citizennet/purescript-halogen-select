module Main where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Web.HTML.HTMLElement (HTMLElement, toElement, fromNode)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toParentNode)
import Web.DOM.Element (getAttribute)
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Data.Array (zipWith)
import Data.Const (Const)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence, traverse, traverse_)
import Data.Tuple (Tuple(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

import Docs.Internal.Proxy (ProxyS, proxy)
import Docs.Internal.Component as Component

-- Finds all nodes labeled "data-component-id" and retrieves the associated attribute.
-- Then, mounts the right component at each node.
main :: Effect Unit
main = HA.runHalogenAff do
  elements <- awaitSelectAll
    { query: QuerySelector "div[data-component]"
    , attr: "data-component"
    }
  flip traverse_ elements $ \e -> runUI app e.attr e.element

----------
-- Routes

type ComponentQuery = ProxyS (Const Void) Unit
type Components m
  = Map.Map String (H.Component HH.HTML ComponentQuery Unit Void m)

routes :: ∀ m. MonadAff m => Components m
routes = Map.fromFoldable
  [ Tuple "typeahead" $ proxy Component.typeahead
  , Tuple "dropdown" $ proxy Component.dropdown ]

data Query a = NoOp a

app :: ∀ m. MonadAff m => H.Component HH.HTML Query String Void m
app =
  H.parentComponent
    { initialState: identity
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render st = do
      let mbComponent = Map.lookup st routes
      case mbComponent of
        Nothing -> HH.div_ []
        Just component -> HH.slot unit component unit absurd

    eval :: Query ~> H.ParentDSL String Query ComponentQuery Unit Void m
    eval (NoOp a) = pure a


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
