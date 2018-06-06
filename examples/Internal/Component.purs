-- | A centralized module ready for use to mount components into documentation pages.

module Docs.Internal.Component where

import Prelude

import Effect.Aff.Class (class MonadAff)
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

----------
-- Built components

typeahead :: ∀ m. MonadAff m => Component m
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

dropdown :: ∀ m. MonadAff m => Component m
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
