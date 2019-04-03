-- | A centralized module ready for use to mount components into documentation pages.

module Docs.Internal.Component where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Docs.Components.Dropdown as Dropdown
import Docs.Components.Typeahead as Typeahead
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

----------
-- Component Types

type State = Unit
type Input = Unit
type Message = Void
type Query = Const Void
type Action = Void

type Component m = H.Component HH.HTML Query Unit Void m
type HalogenM m = H.HalogenM State Action () Void m
type HTML m = H.ComponentHTML Action ChildSlots m

type ChildSlots =
  ( typeahead :: H.Slot Typeahead.Query Typeahead.Message Unit 
  , dropdown :: H.Slot Dropdown.Query Dropdown.Message Unit
  )

_typeahead = SProxy :: SProxy "typeahead"
_dropdown = SProxy :: SProxy "dropdown"

----------
-- Built components

typeahead :: ∀ m. MonadAff m => Component m
typeahead = H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval H.defaultEval
  }
  where
  render :: Unit -> HTML m
  render _ = HH.slot _typeahead unit Typeahead.component { items: users, keepOpen: false } (const Nothing)

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
dropdown = H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval H.defaultEval
  }
  where
  render :: Unit -> HTML m
  render _ = HH.slot _dropdown unit Dropdown.component { items: users } (const Nothing)

  users :: Array String
  users =
    [ "Lyndsey Duffield"
    , "Chris Pine"
    , "Kevin Hart"
    , "Dave Chappelle"
    , "Hannibal Buress"
    , "Rico Suave"
    ]
