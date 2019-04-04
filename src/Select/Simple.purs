-- | This module exposes a component that can be used to build accessible selection
-- | user interfaces. You are responsible for providing all rendering, with the help
-- | of the `Select.Setters` module, but this component provides the relevant
-- | behaviors for dropdowns, autocompletes, typeaheads, keyboard-navigable calendars,
-- | and other selection UIs.
module Select.Simple where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Select as Select

-----
-- Component & Types

type Action item m = Select.Action item () () () m

type Query item m = Select.Query item () () () m

type Message item = Select.Message item ()

type Input item m = Select.Input item () () () m

type State item m = Select.State item () () () m

type Slot item m = H.Slot (Query item m) (Message item)

type Component item m = H.Component HH.HTML (Query item m) (Input item m) (Message item) m

type ComponentHTML item m = H.ComponentHTML (Action item m) () m

type HalogenM item m = H.HalogenM (State item m) (Action item m) () (Message item) m

component :: forall item m. MonadAff m => Component item m
component = H.mkComponent
  { initialState: Select.initialState
  , render: \s@(Select.State st) -> st.render s
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = Select.handleQuery \_ -> pure unit
      , handleAction = Select.handleAction \_ -> pure unit
      , receive = Just <<< Select.receive
      , initialize = Just $ Select.initialize unit
      }
  }

