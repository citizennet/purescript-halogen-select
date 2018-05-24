module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Classy.HTMLElement (getAttribute)
import DOM.HTML (window)
import DOM.HTML.Types (HTMLElement, htmlDocumentToParentNode, readHTMLElement)
import DOM.HTML.Window (document)
import DOM.Node.NodeList (toArray)
import DOM.Node.ParentNode (QuerySelector(QuerySelector), querySelectorAll)
import Data.Array (zipWith)
import Data.Const (Const)
import Data.Either (either)
import Data.Foreign (toForeign)
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
main :: ∀ eff. Eff (HA.HalogenEffects (Component.Effects eff)) Unit
main = HA.runHalogenAff do
  elements <- awaitSelectAll
    { query: QuerySelector "div[data-component]"
    , attr: "data-component"
    }
  flip traverse_ elements $ \e -> runUI app e.attr e.element

----------
-- Routes

type ComponentQuery = ProxyS (Const Void) Unit
type Components m = Map.Map String (H.Component HH.HTML ComponentQuery Unit Void m)

routes :: ∀ eff m. MonadAff ( Component.Effects eff ) m => Components m
routes = Map.fromFoldable
  [ Tuple "typeahead" $ proxy Component.typeahead
  , Tuple "dropdown" $ proxy Component.dropdown ]

data Query a = NoOp a

app :: ∀ eff m. MonadAff ( Component.Effects eff ) m => H.Component HH.HTML Query String Void m
app =
  H.parentComponent
    { initialState: id
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

awaitSelectAll :: ∀ eff
  . { query :: QuerySelector, attr :: String }
 -> Aff (dom :: DOM | eff) (Array { element :: HTMLElement, attr :: String })
awaitSelectAll ask@{ query } = HA.awaitLoad >>= \_ -> selectElements ask >>= pure

selectElements :: ∀ eff
  . { query :: QuerySelector, attr :: String }
 -> Aff (dom :: DOM | eff) (Array { element :: HTMLElement, attr :: String })
selectElements { query, attr } = do
  nodeList <- liftEff
    $ ((querySelectorAll query <<< htmlDocumentToParentNode <=< document) =<< window)

  nodeArray <- liftEff $ toArray nodeList

  let elems :: Array HTMLElement
      elems = fromMaybe []
        $ sequence
        $ either (const Nothing) Just <<< runExcept <<< readHTMLElement <<< toForeign
        <$> nodeArray

  attrs <- liftEff $ traverse (getAttribute attr) elems
  pure $ zipWith ({ element: _, attr: _ }) elems (fromMaybe "" <$> attrs)
