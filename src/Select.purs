-- | This module exposes a component that can be used to build accessible selection
-- | user interfaces. You are responsible for providing all rendering, with the help
-- | of the `Select.Utils.Setters` module, but this component provides the relevant
-- | behaviors for dropdowns, autocompletes, typeaheads, keyboard-navigable calendars,
-- | and other selection UIs.
module Select where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, seeks, store)
import Control.Monad.Aff (Fiber, delay, error, forkAff, killFiber)
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, putVar, takeVar, AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Except (runExcept)
import Data.Array (length, (!!))
import Data.Either (hush)
import Data.Functor.Day (Day, day, runDay)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Data.Newtype (class Newtype, unwrap)
import DOM (DOM)
import DOM.Event.Event (preventDefault, currentTarget)
import DOM.Event.Types as ET
import DOM.Event.KeyboardEvent as KE
import DOM.Event.MouseEvent as ME
import DOM.HTML.HTMLElement (blur, focus)
import DOM.HTML.Types (HTMLElement, readHTMLElement)
import Halogen (Component, ComponentDSL, ComponentHTML, component, liftAff, liftEff, modify) as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.HalogenM (fork, raise) as H
import Select.Internal.State (updateStore, getState)

----------
-- Component Types

-- | A useful shorthand for the Halogen component type
type Component o item eff m
  = H.Component HH.HTML (Query o item eff) (Input o item eff) (Message o item) m

-- | A useful shorthand for the Halogen component HTML type
type ComponentHTML o item eff
  = H.ComponentHTML (Query o item eff)

-- | A useful shorthand for the Halogen component DSL type
type ComponentDSL o item eff m
  = H.ComponentDSL (StateStore o item eff) (Query o item eff) (Message o item) m

-- | The component's state type, wrapped in `Store`. The state and result of the
-- | render function are stored so that `extract` from `Control.Comonad` can be
-- | used to pull out the render function.
type StateStore o item eff
  = Store (State item eff) (ComponentHTML o item eff)

-- | The effects necessary for this component to run. Your component will need to
-- | also support these effects.
type Effects eff = ( avar :: AVAR, dom :: DOM | eff )

----------
-- Core Constructors

-- | These queries ensure the component behaves as expected so long as you use the
-- | helper functions from `Select.Setters.Utils` to attach them to the right elements.
-- |
-- | - `o`: The query type of the component that will mount this component in a child slot.
-- |        This allows you to embed your own queries into the `Select` component.
-- | - `item`: Your custom item type. It can be a simple type like `String`, or something
-- |           complex like `CalendarItem StartDate EndDate (Maybe Disabled)`.
-- | - `eff`: The component's effects.
-- |
-- | - `Search`: Perform a new search with the included string.
-- | - `Highlight`: Change the highlighted index to the next item, previous item, or a
-- |                specific index.
-- | - `Select`: Triggers the "Selected" message for the item at the specified index.
-- | - `CaptureRef`: From an event, captures a reference to the element that triggered
-- |                 the event. Used to manage focus / blur for elements without requiring
-- |                 a particular identifier.
-- | - `TriggerFocus`: Trigger the DOM focus event for the element we have a reference to
-- | - `Key`: Register a key event. `TextInput`-driven components use these only for navigation,
-- |          whereas `Toggle`-driven components also use the key stream for highlighting.
-- | - `PreventClick`: A helper query to prevent click events from bubbling up
-- | - `SetVisibility`: Sets the container visibility to on or off
-- | - `ToggleVisibility`: Toggles the container visibility
-- | - `ReplaceItems`: Replaces all items in state with the new array of items
-- | - `AndThen`: A helper query to evaluate other queries in sequence. Useful when a single
-- |              event (like a click) needs to cause multiple queries to trigger in order.
-- | - `Raise`: A helper query that the component that mounts `Select` can use to embed its
-- |            own queries. Triggers an `Emit` message containing the query when triggered.
-- |            This can be used to easily extend `Select` with more behaviors.
-- | - `Receive`: Sets the component with new input
data Query o item eff a
  = Search String a
  | Highlight Target a
  | Select Int a
  | CaptureRef ET.Event a
  | TriggerFocus a
  | Key KE.KeyboardEvent a
  | PreventClick ME.MouseEvent a
  | SetVisibility Visibility a
  | ToggleVisibility a
  | ReplaceItems (Array item) a
  | AndThen (DayPair (Query o item eff) a)
  | Raise (o Unit) a
  | Receive (Input o item eff) a

-- | A type representing a pair of queries that can be run in order.
newtype DayPair f a = DayPair (Day f f a)
derive instance newtypeDayPair :: Newtype (DayPair f a) _

-- | Helper function for specifying two queries to run, in order.
-- |
-- | ```purescript
-- | [ onClick $ HE.input $ \ev a ->
-- |     H.action (PreventClick ev)
-- |     `andThen`
-- |     TriggerFocus a
-- | ]
-- | ```
andThen :: ∀ o item eff a. Query o item eff Unit -> Query o item eff a -> Query o item eff a
andThen q1 q2 = AndThen $ DayPair $ day (const id) q1 q2

-- | Represents a way to navigate on `Highlight` events: to the previous
-- | item, next item, or the item at a particular index.
data Target = Prev | Next | Index Int
derive instance eqTarget :: Eq Target

-- | Represents whether the component should display the item container. You
-- | should use this in your render function to control visibility:
-- |
-- | ```purescript
-- | render state = if state.visibility == On then renderAll else renderInputOnly
-- | ```
data Visibility = Off | On
derive instance eqVisibility :: Eq Visibility

-- | Text-driven inputs will operate like a normal search-driven selection component.
-- | Toggle-driven inputs will capture key streams and debounce in reverse (only notify
-- | about searches when time has expired).
data InputType
  = TextInput
  | Toggle

-- | The component's state, once unpacked from `Store`.
-- |
-- | - `inputType`: Controls whether the component is input-driven or toggle-driven
-- | - `search`: The text the user has typed into the text input, or stream of keys
-- |             they have typed on the toggle.
-- | - `debounceTime`: How long, in milliseconds, before events should occur based
-- |                   on user searches.
-- | - `debouncer`: A representation of a running timer that, when it expires, will
-- |                trigger debounced events.
-- | - `inputElement`: A reference to the toggle or input element.
-- | - `items`: An array of user-provided `item`s.
-- | - `visibility`: Whether the array of items should be considered visible or not.
-- |                 Useful for rendering.
-- | - `highlightedIndex`: What item in the array of items should be considered
-- |                       highlighted. Useful for rendering.
-- | - `lastIndex`: The length of the array of items.
type State item eff =
  { inputType        :: InputType
  , search           :: String
  , debounceTime     :: Milliseconds
  , debouncer        :: Maybe (Debouncer eff)
  , inputElement     :: Maybe HTMLElement
  , items            :: Array item
  , visibility       :: Visibility
  , highlightedIndex :: Maybe Int
  , lastIndex        :: Int
  }

-- | Represents a running computation that, when it completes, will trigger debounced
-- | effects.
type Debouncer eff =
  { var   :: AVar Unit
  , fiber :: Fiber eff Unit }

-- | The component's input type, which includes the component`s render function. This
-- | render function can also be used to share data with the parent component, as every
-- | time the parent re-renders, the render function will refresh in `Select`.
type Input o item eff =
  { inputType     :: InputType
  , items         :: Array item
  , initialSearch :: Maybe String
  , debounceTime  :: Maybe Milliseconds
  , render        :: State item eff -> ComponentHTML o item eff
  }

-- | The parent is only notified for a few important events, but `Emit` makes it
-- | possible to raise arbitrary queries on events.
-- |
-- | - `Searched`: A new text search has been performed. Contains the text.
-- | - `Selected`: An item has been selected. Contains the item.
-- | - `VisibilityChanged`: The visibility has changed. Contains the new visibility.
-- | - `Emit`: An embedded query has been triggered and can now be evaluated.
-- |           Contains the query.
data Message o item
  = Searched String
  | Selected item
  | VisibilityChanged Visibility
  | Emit (o Unit)

component :: ∀ o item eff m
  . MonadAff (Effects eff) m
 => Component o item (Effects eff) m
component =
  H.component
    { initialState
    , render: extract
    , eval
    , receiver: HE.input Receive
    }
  where
    initialState i = store i.render
      { inputType: i.inputType
      , search: fromMaybe "" i.initialSearch
      , debounceTime: fromMaybe (Milliseconds 0.0) i.debounceTime
      , debouncer: Nothing
      , inputElement: Nothing
      , items: i.items
      , highlightedIndex: Nothing
      , visibility: Off
      , lastIndex: length i.items - 1
      }

    eval :: (Query o item (Effects eff)) ~> ComponentDSL o item (Effects eff) m
    eval = case _ of
      Search str a -> a <$ do
        (Tuple _ st) <- getState
        H.modify $ seeks _ { search = str }
        _ <- eval (SetVisibility On a)

        case st.inputType, st.debouncer of
          TextInput, Nothing -> unit <$ do
            var   <- H.liftAff makeEmptyVar
            fiber <- H.liftAff $ forkAff do
              delay st.debounceTime
              putVar unit var

            -- This compututation will fork and run in the background. When the
            -- var is finally filled, the effect will run (raise a new search)
            _ <- H.fork do
              _ <- H.liftAff $ takeVar var
              H.modify $ seeks _ { debouncer = Nothing, highlightedIndex = Just 0 }
              (Tuple _ newState) <- getState
              H.raise $ Searched newState.search

            H.modify $ seeks _ { debouncer = Just { var, fiber } }

          TextInput, Just debouncer -> do
            let var = debouncer.var
            _ <- H.liftAff $ killFiber (error "Time's up!") debouncer.fiber
            fiber <- H.liftAff $ forkAff do
              delay st.debounceTime
              putVar unit var

            H.modify $ seeks _ { debouncer = Just { var, fiber } }

          -- Key stream is not yet implemented. However, this should capture user
          -- key events and expire their search after a set number of milliseconds.
          _, _ -> pure unit

      Highlight target a -> do
        (Tuple _ st) <- getState
        if st.visibility == Off then pure a else a <$ case target of
          Prev  -> case st.highlightedIndex of
            Just i | i /= 0 -> H.modify $ seeks _ { highlightedIndex = Just (i - 1) }
            _ -> H.modify $ seeks _ { highlightedIndex = Just st.lastIndex }
          Next  -> case st.highlightedIndex of
            Just i | i /= st.lastIndex -> H.modify $ seeks _ { highlightedIndex = Just (i + 1) }
            _ -> H.modify $ seeks _ { highlightedIndex = Just 0 }
          Index i -> H.modify $ seeks _ { highlightedIndex = Just i }

      Select index a -> do
        (Tuple _ st) <- getState
        if st.visibility == Off then pure a else case st.items !! index of
          Just item -> H.raise (Selected item) *> pure a
          _ -> pure a -- Should not be possible.

      CaptureRef event a -> a <$ do
        (Tuple _ st) <- getState
        let elementFromEvent
              = hush
              <<< runExcept
              <<< readHTMLElement
              <<< toForeign
              <<< currentTarget
        H.modify $ seeks _ { inputElement = elementFromEvent event }
        pure a

      TriggerFocus a -> a <$ do
        (Tuple _ st) <- getState
        traverse_ (H.liftEff <<< focus) st.inputElement

      Key ev a -> do
        _ <- eval $ SetVisibility On a
        let prevent = H.liftEff <<< preventDefault <<< KE.keyboardEventToEvent
        case KE.code ev of
         "ArrowUp"   -> prevent ev *> (eval $ Highlight Prev a)
         "ArrowDown" -> prevent ev *> (eval $ Highlight Next a)
         "Escape"    -> a <$ do
           (Tuple _ st) <- getState
           prevent ev
           traverse_ (H.liftEff <<< blur) st.inputElement
         "Enter"     -> a <$ do
           (Tuple _ st) <- getState
           prevent ev
           traverse_ (\index -> eval $ Select index a) st.highlightedIndex
         otherKey    -> pure a

      PreventClick ev a -> a <$ do
        H.liftEff <<< preventDefault <<< ME.mouseEventToEvent $ ev

      SetVisibility v a -> a <$ do
        (Tuple _ st) <- getState
        when (st.visibility /= v) do
          H.modify $ seeks _ { visibility = v, highlightedIndex = Just 0 }
          H.raise $ VisibilityChanged v

      ToggleVisibility a -> a <$ do
        (Tuple _ st) <- getState
        case st.visibility of
          Off -> eval $ SetVisibility On a
          On  -> eval $ SetVisibility Off a

      ReplaceItems items a -> a <$ do
        H.modify $ seeks _
          { items = items
          , lastIndex = length items - 1
          , highlightedIndex = Nothing }

      AndThen dayQuery ->
        unwrap dayQuery # runDay \get fx gy ->
          get <$> eval fx <*> eval gy

      Raise parentQuery a -> a <$ do
        H.raise (Emit parentQuery)

      Receive input a -> a <$ do
        H.modify (updateStore input.render id)

