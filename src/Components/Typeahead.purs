module Select.Components.Typeahead where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Data.Foldable (class Foldable, foldMap)
import Data.Monoid (class Monoid, mempty)
import Data.Either (Either(..))
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.String (Pattern(..), contains)
import Data.Time.Duration (Milliseconds)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Component.ChildPath as CP
import Select.Effects (FX)
import Select.Primitives.Container as Container
import Select.Primitives.Search as Search

----------
-- Motivation

--  This module provides a pre-built typeahead component. It hides the necessary wiring of the Container and Search
--  primitives, and can be used 'out of the box' with a default configuration record. However, it is entirely up
--  to you whether you want to use that record.
--
--  You can also provide handler functions to insert into 'eval' for each important case: a new search from the
--  search field or a new item selection from the container. As always, you are responsible for providing the
--  render functions for each relevant primitive.
--
--  Your handler functions should provide your state management (they should decide how items can be selected, etc.),
--  and this hides the state from the parent unless they explicitly ask for it with `GetSelections`.
--
--  In use: provide render functions, provide configuration or handlers, provide items, and you're good to go.


----------
-- Component Types

-- Newtype because of the self-reference in config function.
newtype State f item e = State
  { items :: f item
  , selections :: f item
  , search :: String
  , slots :: Tuple MountSearch MountContainer
  , config :: Config f item e
  }

--  searchSlot = HH.slot' CP.cp2 unit S.component { render: renderSearch, search: Nothing, debounceTime: Milliseconds 300.0 } ( HE.input HandleSearch )
--  containerSlot = HH.slot' CP.cp1 unit C.component { render: renderContainer, items: testData } ( HE.input HandleContainer )

data Query f item e a
  = HandleContainer (Container.Message (Query f item e) item) a
  | HandleSearch (Search.Message (Query f item e) item) a
  | Remove item a
  | Selections (f item -> a)
  | TypeaheadReceiver (TypeaheadInput f item e) a

-- TODO: Responsible for:
-- input types for search and container primitives (will use these to construct own state)
-- MAYBE: own renderer? better to keep in an unstyled div, right?
-- MAYBE: component + input types for selections slot
type TypeaheadInput f item e =
  { searchPrim :: Search.SearchInput (Query f item e) item e
  , containerPrim :: Container.ContainerInput (Query f item e) item
  , config :: EvalConfig f item e
  }

data TypeaheadMessage item
  = ItemSelected item
  | ItemRemoved item

-- The idea: maintain an Either where you either provide
-- a configuration record, relying on default functionality
-- provided by the component, OR you can provide handlers
-- for the two important child messages (new search or
-- item selected)

type EvalConfig f item e =
  Either (HandlerRecord f item e) ConfigRecord

-- Some standard functionality is baked in to the component
-- and can be configured if the user wants a more 'out of the
-- box' experience. This is a sample.
type ConfigRecord =
  { insertable  :: Boolean        -- If no match, insert?
  , matchType   :: MatchType       -- Function to match
  , keepOpen    :: Boolean        -- Stay open on selection?
  , selectLimit :: SelectLimit    -- One | Limit n | Many
  , duplicates  :: Boolean        -- Allow duplicates?
  }

data MatchType
  = Exact
  | CaseInsensitive
  | Fuzzy

-- Can be used in validation logic
data SelectLimit
  = One
  | Limit Int
  | Many

-- A default config can help minimize their efforts.
defaultConfig :: ConfigRecord
defaultConfig =
  { insertable: false
  , matchType: Fuzzy
  , keepOpen: true
  , selectLimit: Many
  , duplicates: false
  }

-- Alternately, they can provide the full handlers for the
-- two most important queries, with full access to the state
-- and slot types, though this is certainly a more 'advanced'
-- case.

type HandlerRecord f item e =
  { newSearch :: String -> TypeaheadDSL f item e Unit
  , itemSelected :: item -> TypeaheadDSL f item e Unit
  }


----------
-- Convenience component types

type TypeaheadComponent f item e =
  H.Component
    HH.HTML
    (Query f item e)
    (TypeaheadInput f item e)
    (TypeaheadMessage item)
    (FX e)

type TypeaheadHTML f item e =
  H.ParentHTML
    (Query f item e)
    (ChildQuery f item e)
    ChildSlot
    (FX e)

type TypeaheadDSL f item e =
  H.ParentDSL
    (State f item e)
    (Query f item e)
    (ChildQuery f item e)
    ChildSlot
    (TypeaheadMessage item)
    (FX e)


----------
-- Child types

type ContainerQuery f item e =
  Container.ContainerQuery (Query f item e) item

type SearchQuery f item e =
  Search.SearchQuery (Query f item e) item e

type ChildQuery f item e =
  Coproduct2 (ContainerQuery f item e) (SearchQuery f item e)

type ChildSlot =
  Either2 Slot Slot

data Slot = Slot PrimitiveSlot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

data PrimitiveSlot
  = ContainerSlot
  | SearchSlot
derive instance eqPrimitiveSlot :: Eq PrimitiveSlot
derive instance ordPrimitiveSlot :: Ord PrimitiveSlot


----------
-- Component definition

-- You are expected to provide items in some Foldable / Functor instance
-- so our default functions can operate on them. The most common are likely to
-- be Array or Maybe.

component :: ∀ f item e. Applicative f => Foldable f => Monoid (f item) => Show item => TypeaheadComponent f item e
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: HE.input TypeaheadReceiver
    }
  where
    initialState :: TypeaheadInput f item e -> State f item e
    initialState i = State
      { items: i.items
      , debounceTime: i.debounceTime
      , search: fromMaybe "" i.search
      , selections: i.selections
      , render: i.render
      , config: i.config
      }

    render :: State f item e -> TypeaheadHTML f item e
    render _ = HH.span_ []

    eval :: Query f item e ~> TypeaheadDSL f item e
    eval = case _ of

      -- Handle messages from the container.
      HandleContainer message a -> case message of

        -- Evaluate an embedded parent query
        Container.Emit query -> eval query *> pure a

        -- TODO:
        -- Handle a new item selection
        Container.ItemSelected item -> pure a

      -- Handle messages from the search.
      HandleSearch message a -> case message of

        -- Evaluate an embedded parent query
        Search.Emit query -> eval query *> pure a

        -- Route a container query to its correct slot.
        Search.ContainerQuery query -> do
          _ <- H.query' CP.cp1 (Slot ContainerSlot) query
          pure a

        -- TODO
        -- Handle a new search
        Search.NewSearch text -> do
          (State st) <- H.get
          case st.config of
             Left { newSearch } -> newSearch text
             Right _ -> newSearchFn text
          pure a

      -- Handle a 'remove' event on the selections list.
      Remove item a -> pure a

      -- Return the current selections to the parent.
      Selections reply -> do
        (State st) <- H.get
        pure $ reply st.selections

      -- Reset the state with new input.
      TypeaheadReceiver input a -> pure a



----------
-- Helper functions

-- There is no default foldable filter function, but by bringing in the additional
-- applicative and monoid constraints, this becomes possible
filter :: ∀ f item
  . Applicative f => Foldable f => Monoid (f item) =>
  (item -> Boolean) -> f item -> f item
filter p = foldMap (\a -> if p a then pure a else mempty)

----------
-- Helper eval functions

-- These functions feed from the configuration options and allow for a variety of
-- behaviors out of the box. However, if you need more fine-grained control over
-- state and behaviors, you can provide custom handlers.

newSearchFn :: ∀ f item e. Applicative f => Foldable f => Monoid (f item) => Show item => String -> TypeaheadDSL f item e Unit
newSearchFn text = do
  (State st) <- H.get

  let matches = filter (\item -> contains (Pattern text) (show item)) st.items

  -- Update the selections
  H.modify $ \(State st') -> State
    $ st' { search = text
          , items = matches }

  -- Send the new items to the container
  _ <- H.query' CP.cp1 (Slot ContainerSlot)
        $ H.action
        $ Container.Visibility Container.Off

  pure unit
