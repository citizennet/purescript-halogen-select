-- | A centralized module ready for use to mount components into documentation pages.

module Docs.App.Component where
  
import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Maybe (Maybe(..))
import Docs.Components.Calendar as Calendar
import Docs.Components.Dropdown as Dropdown
import Docs.Components.Typeahead as Typeahead
import Halogen as H
import Halogen.HTML as HH

----------
-- Component Types

type State = Unit
type Input = Unit
type Message = Void

data Query a = NoOp a

type Component m = H.Component HH.HTML Query Unit Void m
type DSL q m = H.ParentDSL State Query q Unit Void m
type HTML q m = H.ParentHTML Query q Unit m

type Effects eff = ( console :: CONSOLE, dom :: DOM, now :: NOW, avar :: AVAR, timer :: TIMER | eff )

----------
-- Built components

dropdown :: ∀ eff m. MonadAff ( Effects eff ) m => Component m
dropdown =
  H.parentComponent
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where
    eval :: Query ~> DSL Dropdown.Query m
    eval (NoOp a) = pure a

    render :: Unit -> HTML Dropdown.Query m
    render _ = HH.slot unit Dropdown.component unit absurd

typeahead :: ∀ eff m. MonadAff ( Effects eff ) m => Component m
typeahead =
  H.parentComponent
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where
    eval :: Query ~> DSL Typeahead.Query m
    eval (NoOp a) = pure a

    render :: Unit -> HTML Typeahead.Query m
    render _ = HH.slot unit Typeahead.component users (const Nothing)

    users :: Array String
    users =
      [ "Lyndsey Duffield"
      , "Chris Pine"
      , "Kevin Hart"
      , "Dave Chappelle"
      , "Hannibal Buress"
      , "Rico Suave"
      ]

calendar :: ∀ eff m. MonadAff ( Effects eff ) m => Component m
calendar =
  H.parentComponent
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where
    eval :: Query ~> DSL Calendar.Query m
    eval (NoOp a) = pure a

    render :: Unit -> HTML Calendar.Query m
    render _ = HH.slot unit Calendar.component unit absurd
