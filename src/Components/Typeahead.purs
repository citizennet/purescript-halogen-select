module Select.Components.Typeahead where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Array (filter, (:))
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
newtype State o item e = State
  { items :: Array item
  , selections :: SelectionType item
  , debounceTime :: Milliseconds
  , search :: String
  , slots  :: Slots o item e
  , config :: EvalConfig o item e
  }

type Slots o item e =
  { search    :: TypeaheadHTML o item e
  , container :: TypeaheadHTML o item e }


-- Could also provide 'Limit Int' for restricted lists
data SelectionType item
  = One (Maybe item)
  | Many (Array item)

data Query o item e a
  = HandleContainer (Container.Message o item) a
  | HandleSearch (Search.Message o item) a
  | Remove item a
  | Selections (SelectionType item -> a)
  | TypeaheadReceiver (TypeaheadInput o item e) a

-- TODO: Responsible for:
-- input types for search and container primitives (will use these to construct own state)
-- MAYBE: own renderer? better to keep in an unstyled div, right?
type TypeaheadInput o item e =
  { searchPrim :: Search.SearchInput o item e
  , containerPrim :: Container.ContainerInput o item
  , initialSelection :: SelectionType item
  , config :: EvalConfig o item e
  }

data TypeaheadMessage o item
  = ItemSelected item
  | ItemRemoved item
  | Emit (o Unit)

-- The idea: maintain an Either where you either provide
-- a configuration record, relying on default functionality
-- provided by the component, OR you can provide handlers
-- for the two important child messages (new search or
-- item selected)

type EvalConfig o item e =
  Either (HandlerRecord o item e) ConfigRecord

-- Some standard functionality is baked in to the component
-- and can be configured if the user wants a more 'out of the
-- box' experience. This is a sample.
type ConfigRecord =
  { insertable  :: Boolean        -- If no match, insert?
  , matchType   :: MatchType      -- Function to match
  , keepOpen    :: Boolean        -- Stay open on selection?
  , duplicates  :: Boolean        -- Allow duplicates?
  }

data MatchType
  = Exact
  | CaseInsensitive
  | Fuzzy

-- A default config can help minimize their efforts.
defaultConfig :: ConfigRecord
defaultConfig =
  { insertable: false
  , matchType: Fuzzy
  , keepOpen: true
  , duplicates: false
  }

-- Alternately, they can provide the full handlers for the
-- two most important queries, with full access to the state
-- and slot types, though this is certainly a more 'advanced'
-- case.

type HandlerRecord o item e =
  { newSearch :: String -> TypeaheadDSL o item e Unit
  , itemSelected :: item -> TypeaheadDSL o item e Unit
  }


----------
-- Convenience component types

type TypeaheadComponent o item e =
  H.Component
    HH.HTML
    (Query o item e)
    (TypeaheadInput o item e)
    (TypeaheadMessage o item)
    (FX e)

type TypeaheadHTML o item e =
  H.ParentHTML
    (Query o item e)
    (ChildQuery o item e)
    ChildSlot
    (FX e)

type TypeaheadDSL o item e =
  H.ParentDSL
    (State o item e)
    (Query o item e)
    (ChildQuery o item e)
    ChildSlot
    (TypeaheadMessage o item)
    (FX e)


----------
-- Child types

type ContainerQuery o item =
  Container.ContainerQuery o item

type SearchQuery o item e =
  Search.SearchQuery o item e

type ChildQuery o item e =
  Coproduct2 (ContainerQuery o item) (SearchQuery o item e)

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

component :: ∀ o item e
  . Eq item => Show item => TypeaheadComponent o item e
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: HE.input TypeaheadReceiver
    }
  where
    initialState :: TypeaheadInput o item e -> State o item e
    initialState { searchPrim, containerPrim, initialSelection, config } = State
      { items: containerPrim.items
      , selections: initialSelection
      , debounceTime: searchPrim.debounceTime
      , search: fromMaybe "" searchPrim.search
      , slots:
        { search:
            HH.slot'
              CP.cp2
              (Slot SearchSlot)
              Search.component
              searchPrim
              (HE.input HandleSearch)
        , container:
            HH.slot'
              CP.cp1
              (Slot ContainerSlot)
              Container.component
              containerPrim
              (HE.input HandleContainer)
        }
      , config: config
      }

    render :: State o item e -> TypeaheadHTML o item e
    render (State st) =
      HH.div_ [ st.slots.search
              , st.slots.container ]

    eval :: Query o item e ~> TypeaheadDSL o item e
    eval = case _ of

      -- Handle messages from the container.
      HandleContainer message a -> case message of

        -- Evaluate an embedded parent query
        Container.Emit query -> H.raise (Emit query) *> pure a

        -- TODO:
        -- Handle a new item selection
        Container.ItemSelected item -> a <$ do
          H.raise $ ItemSelected item
          (State st) <- H.get
          case st.config of
             Left { itemSelected } -> itemSelected item
             Right _ -> itemSelectedFn item

      -- Handle messages from the search.
      HandleSearch message a -> case message of

        -- Evaluate an embedded parent query
        Search.Emit query -> H.raise (Emit query) *> pure a

        -- Route a container query to its correct slot.
        Search.ContainerQuery query -> do
          _ <- H.query' CP.cp1 (Slot ContainerSlot) query
          pure a

        -- TODO
        -- Handle a new search
        Search.NewSearch text -> a <$ do
          (State st) <- H.get
          case st.config of
             Left { newSearch } -> newSearch text
             Right _ -> newSearchFn text

      -- Handle a 'remove' event on the selections list.
      Remove item a -> a <$ do
        (State st) <- H.get

        let newSelections = st.selections
            newItems = item : st.items

        H.modify \(State st') -> State
          $ st' { items = newItems
                , selections = newSelections }

        H.raise $ ItemRemoved item

      -- Return the current selections to the parent.
      Selections reply -> do
        (State st) <- H.get
        pure $ reply st.selections

      -- Overwrite the state with new input.
      TypeaheadReceiver input a -> a <$ do
        H.put (initialState input)


----------
-- Helper eval functions

-- These functions feed from the configuration options and allow for a variety of
-- behaviors out of the box. However, if you need more fine-grained control over
-- state and behaviors, you can provide custom handlers.


-- Searching requires the ability to compare the item to a string. We can require a Show
-- instance, or perhaps our own type class for "CompareString", where you can provide
-- some function to turn your item into a string, including just "show"
newSearchFn :: ∀ o item e. Eq item => Show item => String -> TypeaheadDSL o item e Unit
newSearchFn text = do
  (State st) <- H.get

  let matches = filter (\item -> contains (Pattern text) (show item)) st.items

  -- Update the selections
  H.modify $ \(State st') -> State $ st' { search = text }

  -- Send the new items to the container
  _ <- H.query' CP.cp1 (Slot ContainerSlot)
        $ H.action
        $ Container.ReplaceItems matches

  pure unit


-- Manages a selection depending on what kind of select this is.
itemSelectedFn :: ∀ o item e. Eq item => item -> TypeaheadDSL o item e Unit
itemSelectedFn item = do
  (State st) <- H.get

  let (Tuple newSelections newItems) = case st.selections of
        One Nothing  -> Tuple (One $ Just item) (filter ((/=) item) st.items)
        One (Just i) -> Tuple (One $ Just item) ((:) i $ filter ((/=) item) st.items)
        Many xs      -> Tuple (Many $ item : xs) (filter ((/=) item) st.items)

  H.modify \(State st') -> State $ st' { selections = newSelections }

  -- Send the new items to the container
  _ <- H.query' CP.cp1 (Slot ContainerSlot)
        $ H.action
        $ Container.ReplaceItems newItems

  pure unit
