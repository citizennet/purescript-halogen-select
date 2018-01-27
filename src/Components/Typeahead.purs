module Select.Components.Typeahead where

import Prelude

import Data.Monoid (class Monoid)
import Data.Maybe (Maybe, fromMaybe)
import Data.Either (Either(..))
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Component.ChildPath as CP
import Select.Effects (FX)
import Select.Primitives.Container as Container
import Select.Primitives.Search as Search

----------
-- Component Types

-- Newtype because of the self-reference in config function.
newtype State f item e = State
  { items :: f item
  , selections :: f item
  , search :: String
  , config :: TypeaheadConfig f item e
  }

data Query f item e a
  = HandleContainer (Container.Message (Query f item e) item) a
  | HandleSearch (Search.Message (Query f item e) item) a
  | Remove item a
  | Selections (f item -> a)
  | TypeaheadReceiver (TypeaheadInput f item e) a

-- TODO: Responsible for:
-- component + input types for search slot
-- component + input types for container slot
-- component + input types for selections slot
-- render function for overall typeahead
type TypeaheadInput f item e =
  { items :: f item
  , search :: Maybe String
  , selections :: f item
  , config :: TypeaheadConfig f item e
  }

data TypeaheadMessage item
  = ItemSelected item
  | ItemRemoved item


-- The idea: maintain an Either where you either provide
-- a configuration record, relying on default functionality
-- provided by the component, OR you can provide handlers
-- for the two important child messages (new search or
-- item selected)

type TypeaheadConfig f item e =
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

component :: ∀ f item e. Functor f => Monoid item => TypeaheadComponent f item e
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
      , search: fromMaybe "" i.search
      , selections: i.selections
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

        -- TODO
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
-- Helper eval functions

-- These functions feed from the configuration options and allow for a variety of
-- behaviors out of the box. However, if you need more fine-grained control over
-- state and behaviors, you can provide custom handlers.

newSearchFn :: ∀ f item e. Functor f => String -> TypeaheadDSL f item e Unit
newSearchFn text = do
  H.modify $ \(State st) -> State $ st { search = text }
--
  -- Send the new items to the container
  _ <- H.query' CP.cp1 (Slot ContainerSlot)
        $ H.action
        $ Container.Visibility Container.Off
--
  pure unit
