-- | A centralized module ready for use to mount components into documentation pages.

module Docs.Internal.Component where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH

import Docs.Components.Typeahead as Typeahead
import Docs.Components.Dropdown as Dropdown

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
    render _ = HH.slot unit Typeahead.component { items: users, keepOpen: false } (const Nothing)

    users :: Array String
    users =
      [ "Lyndsey Duffield"
      , "Chris Pine"
      , "Kevin Hart"
      , "Dave Chappelle"
      , "Hannibal Buress"
      , "Rico Suave"
      ]

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
    render _ = HH.slot unit Dropdown.component { items: users } (const Nothing)

    users :: Array String
    users =
      [ "Lyndsey Duffield"
      , "Chris Pine"
      , "Kevin Hart"
      , "Dave Chappelle"
      , "Hannibal Buress"
      , "Rico Suave"
      ]
