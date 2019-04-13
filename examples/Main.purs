module Main where

import Prelude

import Data.Array (zipWith, length)
import Data.Const (Const)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (for_, sequence, traverse)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Docs.Internal.Proxy (ProxyS, proxy)
import Docs.Internal.RemoteData (RemoteData(..), toMaybe)
import Docs.Components.Typeahead as Typeahead
import Docs.Components.Dropdown as Dropdown
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Select as Select
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
main = HA.runHalogenAff do
  elements <- awaitSelectAll
    { query: QuerySelector "div[data-component]"
    , attr: "data-component"
    }
  for_ elements \e -> runUI app e.attr e.element

----------
-- Routes

type Components
  = M.Map String (H.Component HH.HTML (ProxyS (Const Void) Unit) Unit Void Aff)

routes :: Components
routes = M.fromFoldable
  [ Tuple "typeahead" $ proxy typeahead
  , Tuple "dropdown" $ proxy dropdown 
  ]

app :: H.Component HH.HTML (Const Void) String Void Aff
app = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
  }
  where
  render st = M.lookup st routes # case _ of
    Nothing -> HH.div_ []
    Just component -> HH.slot (SProxy :: SProxy "child") unit component unit absurd

----------
-- Selection Helpers

awaitSelectAll
  :: { query :: QuerySelector, attr :: String }
  -> Aff (Array { element :: HTMLElement, attr :: String })
awaitSelectAll ask@{ query } = HA.awaitLoad >>= \_ -> selectElements ask

selectElements
  :: { query :: QuerySelector, attr :: String }
  -> Aff (Array { element :: HTMLElement, attr :: String })
selectElements { query, attr } = do
  nodeArray <- liftEffect do 
    toArray =<< querySelectorAll query <<< toParentNode =<< document =<< window
  let 
    elems = fromMaybe [] <<< sequence $ fromNode <$> nodeArray
  attrs <- liftEffect $ traverse (getAttribute attr <<< toElement) elems
  pure $ zipWith ({ element: _, attr: _ }) elems (fromMaybe "" <$> attrs)

----------
-- Components

dropdown :: forall t0 t1 t2. H.Component HH.HTML t0 t1 t2 Aff
dropdown = H.mkComponent
  { initialState: const unit
  , render: \_ ->
      HH.slot label unit (Select.component Dropdown.spec) input \_ -> Nothing
  , eval: H.mkEval H.defaultEval
  }
  where
  label = SProxy :: SProxy "dropdown"
  input = 
    { inputType: Select.Toggle
    , debounceTime: Nothing
    , search: Nothing
    , getItemCount: length <<< _.items
    , items: [ "one", "two", "three" ]
    , selection: Nothing
    }

typeahead :: forall t0 t1 t2. H.Component HH.HTML t0 t1 t2 Aff
typeahead = H.mkComponent
  { initialState: const unit
  , render: \_ ->
      HH.slot label unit (Select.component Typeahead.spec) input \_ -> Nothing
  , eval: H.mkEval H.defaultEval
  }
  where
  label = SProxy :: SProxy "typeahead"
  input = 
    { inputType: Select.Text
    , debounceTime: Just (Milliseconds 300.0)
    , search: Nothing
    , getItemCount: maybe 0 length <<< toMaybe <<< _.available
    , selections: mempty
    , available: NotAsked
    }
