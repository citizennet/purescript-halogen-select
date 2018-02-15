module Example.Content.Home where

import Prelude

import Control.Monad.Eff.Now (NOW)
import Control.Monad.Aff.Class (class MonadAff)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Component.ChildPath as CP
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)

import Example.Component.Dropdown as Dropdown
import Example.Component.Typeahead as Typeahead
import Example.Component.Calendar as Calendar

type Effects e = ( now :: NOW | Typeahead.Effects e)

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
      HH.div
      [ class_ "leading-normal" ]
      [ header, subhead, container components, footer ]

    eval :: Query ~> H.ParentDSL Unit Query ChildQuery ChildSlot Void _
    eval (HandleTypeahead _ next) = pure next


----------
-- Rendering

class_ = HP.class_ <<< HH.ClassName

header :: ∀ i p. HH.HTML i p
header =
  HH.div
  [ class_ "bg-grey-lighter" ]
  [ HH.div
    [ class_ "container max-w-md mx-auto py-16 px-4" ]
    [ HH.h1
    [ class_ "text-6xl text-grey-darkest mb-6 font-bold" ]
      [ HH.text "Halogen Select" ]
    , HH.p
      [ class_ "text-grey-darker text-xl font-medium mb-4" ]
      [ HH.text "Primitive components for selection user interfaces like calendars, typeaheads, dropdowns, and image pickers." ]
    ]
  ]

subhead :: ∀ i p. HH.HTML i p
subhead =
  HH.div
  [ class_ "bg-grey-lightest" ]
  [ HH.div
    [ class_ "container max-w-md mx-auto py-16 px-4" ]
    [ HH.a
      [ HP.href "https://github.com/citizennet/purescript-halogen-select"
      , class_ "text-grey-darkest hover_underline" ]
      [ HH.text "Star the repo on GitHub." ]
    ]
  ]

-- container :: ∀ i p. Array (HH.HTML i p) -> HH.HTML i p
container components =
  HH.div
  [ class_ "container max-w-md mx-auto py-16 px-4" ]
  ( card <$> components )

card { title, description, component } =
  HH.div
  [ class_ "mb-8" ]
  [ HH.h1_
    [ HH.text title ]
  , HH.div
    [ class_ "text-xl text-grey-dark mb-4 mt-2" ]
    [ HH.text description ]
  , component ]

components = [ dropdown, typeahead, calendar ]

dropdown =
  { title: "Dropdown"
  , description: "A simple dropdown toggle"
  , component: dropdownComponent }

typeahead =
  { title: "Typeahead"
  , description: "A simple multi-select typeahead"
  , component: typeaheadComponent }

calendar =
  { title: "Calendar"
  , description: "A simple selectable calendar"
  , component: calendarComponent }

componentBlock { slot, docs } =
  HH.div
  [ class_ "rounded border-2 border-grey-light bg-white" ]
  [ HH.div
    [ class_ "p-4 bg-grey-lightest" ]
    [ slot ]
  , HH.div
    [ class_ "p-4 border-t-2 border-grey-light" ]
    [ docs ]
  ]

dropdownComponent = componentBlock
  { slot: HH.slot' CP.cp1 unit Dropdown.component unit absurd
  , docs: docs }
  where
    docs =
      HH.p
      [ class_ "font-mono text-grey-darkest" ]
      [ HH.text "HH.slot' CP.cp1 unit Dropdown.component unit absurd" ]

typeaheadComponent = componentBlock
  { slot: HH.slot' CP.cp2 unit Typeahead.component dev (HE.input HandleTypeahead)
  , docs: docs }
  where
    docs =
      HH.p
      [ class_ "font-mono text-grey-darkest" ]
      [ HH.text "HH.slot' CP.cp2 unit Typeahead.Component myData absurd" ]

calendarComponent = componentBlock
  { slot: HH.slot' CP.cp3 unit Calendar.component unit absurd
  , docs: docs }
  where
    docs =
      HH.p
      [ class_ "font-mono text-grey-darkest" ]
      [ HH.text "HH.slot' CP.cp3 unit Calendar.component unit absurd" ]

footer :: ∀ i p. HH.HTML i p
footer =
  HH.div
  [ class_ "bg-grey-lighter py-8 px-4" ]
  [ HH.div
    [ class_ "container max-w-md mx-auto px-4 text-grey-darker" ]
    [ HH.text "Proudly open-sourced by CitizenNet, a Conde Nast company." ]
  ]


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


