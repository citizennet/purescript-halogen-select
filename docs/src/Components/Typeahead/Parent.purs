module Docs.Components.Typeahead.Parent where

import Prelude

import Control.Monad.Eff.Now (NOW)
import Control.Monad.Aff.Class (class MonadAff)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Docs.Components.Typeahead as Child

type Effects e = ( now :: NOW | Child.Effects e)

data Query a
  = HandleTypeahead Int Child.Message a

type ChildSlot = Slot
type ChildQuery = Child.Query

data Slot = Slot Int
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

component :: ∀ m e
  . MonadAff ( Effects e ) m
 => H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: Unit -> H.ParentHTML Query ChildQuery ChildSlot m
    render _ =
      HH.div_
        [ HH.slot (Slot 1) Child.component pres (HE.input (HandleTypeahead 1))
        , HH.slot (Slot 2) Child.component dev (HE.input (HandleTypeahead 2)) ]

    eval :: Query ~> H.ParentDSL Unit Query ChildQuery ChildSlot Void m
    eval (HandleTypeahead _ _ next) = pure next


----------
-- Data

pres :: Array String
pres =
  [ "Barack Obama"
  , "George W. Bush"
  , "Bill Clinton"
  , "George H. W. Bush"
  ]

dev :: Array String
dev =
  [ "Thomas Honeyman"
  , "Dave Zuch"
  , "Chris Cornwell"
  , "Forest Toney"
  , "Lee Leathers"
  , "Kim Wu"
  , "Rachel Blair"
  , "Tara Strauss"
  , "Sanket Sabnis"
  , "Aaron Chu"
  , "Vincent Busam"
  , "Riley Gibbs"
  ]

