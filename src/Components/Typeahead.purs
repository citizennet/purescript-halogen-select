module Select.Components.Typeahead where

import Prelude

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

-- TODO:
-- This will need to change to Store because the render
-- function will also be passed in.
type State item e =
  { items :: Array item
  , selections :: Array item
  , search :: String
  , config :: TypeaheadConfig item e  }

data Query item e a
  = HandleContainer (Container.Message (Query item e) item) a
  | HandleSearch (Search.Message (Query item e) item) a
  | Remove item a
  | Selections (Array item -> a)
  | TypeaheadReceiver (TypeaheadInput item e) a

-- TODO: Responsible for:
-- component + input types for search slot
-- component + input types for container slot
-- component + input types for selections slot
-- render function for overall typeahead
type TypeaheadInput item e =
  { items :: Array item
  , config :: TypeaheadConfig item e
  }

data TypeaheadMessage item
  = ItemSelected item
  | ItemRemoved item
  | GetSelections (Array item)


-- The idea: maintain an Either where you either provide
-- a configuration record, relying on default functionality
-- provided by the component, OR you can provide handlers
-- for the two important child messages (new search or
-- item selected)

type TypeaheadConfig item e =
  Either (HandlerRecord item e) (ConfigRecord item)

-- Some standard functionality is baked in to the component
-- and can be configured if the user wants a more 'out of the
-- box' experience. This is a sample.
type ConfigRecord item =
  { insertable  :: Boolean        -- If no match, insert?
  , matchType   :: MatchType item -- Function to match
  , keepOpen    :: Boolean        -- Stay open on selection?
  , selectLimit :: SelectLimit    -- One | Limit n | Many
  , duplicates  :: Boolean        -- Allow duplicates?
  }

data MatchType item
  = Exact
  | CaseInsensitive
  | Fuzzy
  | Custom (String -> Array item -> Array item)

data SelectLimit
  = One
  | Limit Int
  | Many

-- A default config can help minimize their efforts.
defaultConfig :: ∀ item. ConfigRecord item
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
type HandlerRecord item e =
  { newSearch :: Query item e ~> TypeaheadDSL item e
  , itemSelected :: Query item e ~> TypeaheadDSL item e
  }


----------
-- Convenience component types

type TypeaheadComponent item e =
  H.Component
    HH.HTML
    (Query item e)
    (TypeaheadInput item e)
    (TypeaheadMessage item)
    (FX e)

type TypeaheadHTML item e =
  H.ParentHTML
    (Query item e)
    (ChildQuery item e)
    ChildSlot
    (FX e)

type TypeaheadDSL item e =
  H.ParentDSL
    (State item e)
    (Query item e)
    (ChildQuery item e)
    ChildSlot
    (TypeaheadMessage item)
    (FX e)

----------
-- Child types

type ContainerQuery item e =
  Container.ContainerQuery (Query item e) item

type SearchQuery item e =
  Search.SearchQuery (Query item e) item e

type ChildQuery item e =
  Coproduct2 (ContainerQuery item e) (SearchQuery item e)

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

component :: ∀ item e. TypeaheadComponent item e
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: HE.input TypeaheadReceiver
    }
  where
    initialState :: TypeaheadInput item e -> State item e
    initialState i =
      { items: i.items
      , search: ""
      , selections: []
      , config: i.config }

    render :: (State item e) -> TypeaheadHTML item e
    render state = HH.span_ []

    eval :: Query item e ~> TypeaheadDSL item e
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
        Search.NewSearch text -> pure a

      -- Handle a 'remove' event on the selections list.
      Remove item a -> pure a

      -- Return the current selections to the parent.
      Selections reply -> do
        st <- H.get
        pure $ reply st.selections

      -- Reset the state with new input.
      TypeaheadReceiver input a -> pure a
