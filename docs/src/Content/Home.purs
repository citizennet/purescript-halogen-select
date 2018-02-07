module Example.Content.Home where

import Prelude

import Control.Monad.Eff.Now (NOW)
import Control.Monad.Aff.Class (class MonadAff)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Component.ChildPath as CP
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)

import Example.Component.Dropdown as Dropdown
import Example.Component.Typeahead as Typeahead
import Example.Component.Calendar as Calendar

type Effects e = ( now :: NOW | Typeahead.TypeaheadEffects e)

data Query a
  = HandleTypeahead Typeahead.Message a

type ChildQuery = Coproduct3 Dropdown.Query Typeahead.Query Calendar.Query
type ChildSlot = Either3 Unit Unit Unit

component :: ∀ m e. H.Component HH.HTML Query Unit Void _
component =
  H.parentComponent
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: Unit -> H.ParentHTML Query ChildQuery ChildSlot _
    render _ =
      HH.div_
        [ HH.slot' CP.cp1 unit Dropdown.component unit absurd
        , HH.slot' CP.cp2 unit Typeahead.component dev (HE.input HandleTypeahead)
        , HH.slot' CP.cp3 unit Calendar.component unit absurd ]

    eval :: Query ~> H.ParentDSL Unit Query ChildQuery ChildSlot Void _
    eval (HandleTypeahead _ next) = pure next


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


