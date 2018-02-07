module Main where

import Prelude

import Control.Coroutine (($$))
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (popstate)
import DOM.HTML.Location (pathname, search)
import DOM.HTML.Types (windowToEventTarget)
import DOM.HTML.Window (location)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Example.Routes (Location, pathFromLocation, locationFromPath, app, Query(..))

main = HA.runHalogenAff do

  -- Get the current path from the window object
  loc <- liftEff getRoute

  -- Wait for the document to load and grab the body element
  body <- HA.awaitBody

  -- Run the application
  io <- runUI (app loc) unit body

  -- Listen for popstate events
  CR.runProcess (pushStateProducer $$ pushStateConsumer io.query)


----------
-- Helpers

getRoute :: ∀ eff. Eff (dom :: DOM, console :: CONSOLE | eff) Location
getRoute = do
  loc <- location =<< window
  path <- pathname loc
  search' <- search loc
  log $ path <> search'
  log $ show $ locationFromPath $ path <> search'
  pure $ locationFromPath $ path <> search'


----------
-- Push State

-- A producer coroutine that emits messages whenever the window emits a
-- popstate event.
pushStateProducer
  :: ∀ eff
   . CR.Producer
      Location
      (Aff (avar :: AVAR, dom :: DOM, console :: CONSOLE | eff))
      Unit
pushStateProducer = CRA.produce \emit ->
  let
    route = do
      r <- getRoute
      emit $ Left $ r
  in
    window
      >>= windowToEventTarget
      >>> addEventListener popstate (eventListener $ pure route) false


-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `ChangeLocation` queries in when it receives inputs from the
-- producer.
--  pushStateConsumer
--    :: ∀ eff
--     . (Query ~> Aff (HA.HalogenEffects eff))
--    -> CR.Consumer
--        Location
--        (Aff (HA.HalogenEffects eff))
--        Unit
pushStateConsumer query = CR.consumer \event -> do
  query $ H.action $ ChangeLocation event
  pure Nothing
