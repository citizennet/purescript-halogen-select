module Example.Routes where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Routing (match)
import Routing.Match (Match)
import Routing.Match.Class (lit, end)
import Control.Monad.Aff.Console (log, CONSOLE)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (popstate)
import DOM.HTML.Location (pathname, search, protocol, host)
import DOM.HTML.Types (HISTORY, windowToEventTarget)
import DOM.HTML.Window (location, history)
import DOM.HTML.History (DocumentTitle(DocumentTitle), URL(URL), pushState)
import Data.Foreign (toForeign)

----------
-- Locations

data Location
  = Home
  | Guide
  | Components
  | Tutorials
  | Documentation
  | NotFound

instance showLocation :: Show Location where
  show Home = "Home"
  show Guide = "Guide"
  show Components = "Components"
  show Tutorials = "Tutorials"
  show Documentation = "Docs"
  show NotFound = "NotFound"

pathFromLocation :: Location -> String
pathFromLocation = case _ of
  Home -> "/"
  Guide -> "/guide"
  Components -> "/components"
  Tutorials -> "/tutorials"
  Documentation -> "/docs"
  NotFound -> "/404"


----------
-- Routing

locationFromPath :: String -> Location
locationFromPath = either (\_ -> NotFound) id <<< match router

router :: Match Location
router =
  Home <$ (lit "" *> lit "" *> end)
  <|>
  Guide <$ (lit "" *> lit "guide")
  <|>
  Components <$ (lit "" *> lit "components")
  <|>
  Tutorials <$ (lit "" *> lit "tutorials")
  <|>
  Documentation <$ (lit "" *> lit "docs")

----------
-- Component

type State =
  { location :: Location }

data Query a
  = ChangeLocation Location a
  | PushLocation Location a
  | Initialize a

type Slot = String

----------
--

-- The render should basically look up the current location and render
-- based on that. The top-level component in the entire application
-- is the router, which here relies on pushState. Because of that, link
-- clicks shouldn't use the usual onClick but should use `preventDefault`
-- (right?)

app :: ∀ eff m
  . MonadAff (dom :: DOM, history :: HISTORY, console :: CONSOLE | eff) m
  => Location
  -> H.Component HH.HTML Query Unit Void m
app location =
  H.lifecycleParentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    }
  where
  initialState :: Unit -> State
  initialState _ = { location: location }

  render :: State -> _
  render st = case st.location of
    Home -> HH.div_ [ HH.text "home" ]
    other -> HH.div_ [ ]

  eval :: Query ~> H.ParentDSL State Query _ Slot Void m
  eval = case _ of
    -- ChangeLocation handles the case where a popstate event has occurred but the user
    -- did not click to navigate. This happens when the user uses the back arrow.
    -- In this case, change the route, but do not modify the history object.
    ChangeLocation loc next -> do
      H.modify _ { location = loc }
      pure next

    -- PushLocation handles the case where the user clicks to navigate. Use pushState
    -- to push the current route onto the history object and update the state
    PushLocation loc next -> do
      H.modify _ { location = loc }
      liftEff $ do
        h <- history =<< window
        pushState (toForeign {}) (DocumentTitle "Select ") (URL $ pathFromLocation loc) h
      pure next

    Initialize next -> do
      st <- H.get
      H.liftAff $ log $ show st.location
      pure next
