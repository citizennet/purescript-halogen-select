-- | This module exposes a component that can be used to build accessible selection
-- | user interfaces. You are responsible for providing all rendering, with the help
-- | of the `Select.Utils.Setters` module, but this component provides the relevant
-- | behaviors for dropdowns, autocompletes, typeaheads, keyboard-navigable calendars,
-- | and other selection UIs.
module Select where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, seeks, store)
import Effect.Aff (Fiber, delay, error, forkAff, killFiber)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Foreign (unsafeToForeign, unsafeFromForeign)
import Control.Monad.Except (runExcept)
import Control.Monad.Free (Free, foldFree, liftF)
import Web.Event.Event (preventDefault, currentTarget, Event)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME
import Web.HTML.HTMLElement (HTMLElement, blur, focus)
import Data.Array (length, (!!))
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_, traverse_)
import Data.Tuple (Tuple(..))
import Halogen (Component, ComponentDSL, ComponentHTML, component, liftAff, liftEffect, modify) as H
import Halogen.HTML as HH
import Halogen.Query.HalogenM (fork, raise) as H
import Select.Internal.State (updateStore, getState)

----------
-- Component Types

-- | A useful shorthand for the Halogen component type
type Component o item m
  = H.Component HH.HTML (Query o item) (Input o item) (Message o item) m

-- | A useful shorthand for the Halogen component HTML type
type ComponentHTML o item
  = H.ComponentHTML (Query o item)

-- | A useful shorthand for the Halogen component DSL type
type ComponentDSL o item m
  = H.ComponentDSL (StateStore o item) (Query o item) (Message o item) m

-- | The component's state type, wrapped in `Store`. The state and result of the
-- | render function are stored so that `extract` from `Control.Comonad` can be
-- | used to pull out the render function.
type StateStore o item
  = Store (State item) (ComponentHTML o item)

----------
-- Core Constructors

-- | These queries ensure the component behaves as expected so long as you use the
-- | helper functions from `Select.Setters.Utils` to attach them to the right elements.
-- |
-- | - `o`: The query type of the component that will mount this component in a child slot.
-- |        This allows you to embed your own queries into the `Select` component.
-- | - `item`: Your custom item type. It can be a simple type like `String`, or something
-- |           complex like `CalendarItem StartDate EndDate (Maybe Disabled)`.
-- |
-- | See the below functions for documentation for the individual constructors.
-- | The README details how to use them in Halogen code, since the patterns
-- | are a little different.
data QueryF o item a
  = Search String a
  | Highlight Target a
  | Select Int a
  | CaptureRef Event a
  | Focus Boolean a
  | Key KE.KeyboardEvent a
  | PreventClick ME.MouseEvent a
  | SetVisibility Visibility a
  | GetVisibility (Visibility -> a)
  | ReplaceItems (Array item) a
  | Raise (o Unit) a
  | Receive (Input o item) a

type Query o item = Free (QueryF o item)

-- | Trigger the relevant action with the event each time it occurs
always :: ∀ a b. a -> b -> Maybe a
always = const <<< Just

-- | Perform a new search with the included string.
search :: ∀ o item. String -> Query o item Unit
search s = liftF (Search s unit)

-- | Change the highlighted index to the next item, previous item, or a
-- | specific index.
highlight :: ∀ o item. Target -> Query o item Unit
highlight t = liftF (Highlight t unit)

-- | Triggers the "Selected" message for the item at the specified index.
select :: ∀ o item. Int -> Query o item Unit
select i = liftF (Select i unit)

-- | From an event, captures a reference to the element that triggered the
-- | event. Used to manage focus / blur for elements without requiring a
-- | particular identifier.
captureRef :: ∀ o item. Event -> Query o item Unit
captureRef r = liftF (CaptureRef r unit)

-- | Trigger the DOM focus event for the element we have a reference to.
triggerFocus :: ∀ o item . Query o item Unit
triggerFocus = liftF (Focus true unit)

-- | Trigger the DOM blur event for the element we have a reference to
triggerBlur :: ∀ o item . Query o item Unit
triggerBlur = liftF (Focus false unit)

-- | Register a key event. `TextInput`-driven components use these only for
-- | navigation, whereas `Toggle`-driven components also use the key stream for
-- | highlighting.
key :: ∀ o item . KE.KeyboardEvent -> Query o item Unit
key e = liftF (Key e unit)

-- | A helper query to prevent click events from bubbling up.
preventClick :: ∀ o item . ME.MouseEvent -> Query o item Unit
preventClick i = liftF (PreventClick i unit)

-- | Set the container visibility (`On` or `Off`)
setVisibility :: ∀ o item . Visibility -> Query o item Unit
setVisibility v = liftF (SetVisibility v unit)

-- | Get the container visibility (`On` or `Off`). Most useful when sequenced
-- | with other actions.
getVisibility :: ∀ o item . Query o item Visibility
getVisibility = liftF (GetVisibility identity)

-- | Toggles the container visibility.
toggleVisibility :: ∀ o item . Query o item Unit
toggleVisibility = getVisibility >>= not >>> setVisibility

-- | Replaces all items in state with the new array of items.
replaceItems :: ∀ o item . Array item -> Query o item Unit
replaceItems items = liftF (ReplaceItems items unit)

-- | A helper query that the component that mounts `Select` can use to embed its
-- | own queries. Triggers an `Emit` message containing the query when triggered.
-- | This can be used to easily extend `Select` with more behaviors.
raise :: ∀ o item . o Unit -> Query o item Unit
raise o = liftF (Raise o unit)

-- | Sets the component with new input.
receive :: ∀ o item . Input o item -> Query o item Unit
receive i = liftF (Receive i unit)

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
-- |
-- | This is a Boolean Algebra, where `On` corresponds to true, and `Off` to
-- | false, as one might expect. Thus, `not` will invert visibility.
data Visibility = Off | On
derive instance eqVisibility :: Eq Visibility
derive instance ordVisibility :: Ord Visibility

instance heytingAlgebraVisibility :: HeytingAlgebra Visibility where
  tt = On
  ff = Off
  not On = Off
  not Off = On
  conj On On = On
  conj _ _ = Off
  disj Off Off = Off
  disj _ _ = On
  implies On Off = Off
  implies _ _ = On
instance booleanAlgebraVisibility :: BooleanAlgebra Visibility

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
type State item =
  { inputType        :: InputType
  , search           :: String
  , debounceTime     :: Milliseconds
  , debouncer        :: Maybe Debouncer
  , inputElement     :: Maybe HTMLElement
  , items            :: Array item
  , visibility       :: Visibility
  , highlightedIndex :: Maybe Int
  , lastIndex        :: Int
  }

-- | Represents a running computation that, when it completes, will trigger debounced
-- | .cts.
type Debouncer =
  { var   :: AVar Unit
  , fiber :: Fiber Unit }

-- | The component's input type, which includes the component's render function. This
-- | render function can also be used to share data with the parent component, as every
-- | time the parent re-renders, the render function will refresh in `Select`.
type Input o item =
  { inputType     :: InputType
  , items         :: Array item
  , initialSearch :: Maybe String
  , debounceTime  :: Maybe Milliseconds
  , render        :: State item -> ComponentHTML o item
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

component :: ∀ o item m
  . MonadAff m
 => Component o item m
component =
  H.component
    { initialState
    , render: extract
    , eval: eval'
    , receiver: Just <<< receive
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

    -- Construct the fold over the free monad based on the stepwise eval
    eval' :: Query o item ~> ComponentDSL o item m
    eval' a = foldFree eval a

    -- Helper for setting visibility inside `eval`. Eta-expanded bc strict
    -- mutual recursion woes.
    setVis v = eval' (setVisibility v)

    -- Just the normal Halogen eval
    eval :: QueryF o item ~> ComponentDSL o item m
    eval = case _ of
      Search str a -> a <$ do
        (Tuple _ st) <- getState
        H.modify $ seeks _ { search = str }
        setVis On

        case st.inputType, st.debouncer of
          TextInput, Nothing -> unit <$ do
            var   <- H.liftAff AVar.empty
            fiber <- H.liftAff $ forkAff do
              delay st.debounceTime
              AVar.put unit var

            -- This compututation will fork and run in the background. When the
            -- var is finally filled, the .ct will run (raise a new search)
            _ <- H.fork do
              _ <- H.liftAff $ AVar.take var
              H.modify $ seeks _ { debouncer = Nothing, highlightedIndex = Just 0 }
              (Tuple _ newState) <- getState
              H.raise $ Searched newState.search

            H.modify $ seeks _ { debouncer = Just { var, fiber } }

          TextInput, Just debouncer -> do
            let var = debouncer.var
            _ <- H.liftAff $ killFiber (error "Time's up!") debouncer.fiber
            fiber <- H.liftAff $ forkAff do
              delay st.debounceTime
              AVar.put unit var

            H.modify $ seeks _ { debouncer = Just { var, fiber } }

          -- Key stream is not yet implemented. However, this should capture user
          -- key events and expire their search after a set number of milliseconds.
          _, _ -> pure unit

      Highlight target a -> a <$ do
        (Tuple _ st) <- getState
        when (st.visibility /= Off) $ do
          let highlightedIndex = case target of
                Prev  -> case st.highlightedIndex of
                  Just i | i /= 0 ->
                    Just (i - 1)
                  _ ->
                    Just st.lastIndex
                Next  -> case st.highlightedIndex of
                  Just i | i /= st.lastIndex ->
                    Just (i + 1)
                  _ ->
                    Just 0
                Index i ->
                  Just i
          H.modify $ seeks _ { highlightedIndex = highlightedIndex }

      Select index a -> a <$ do
        (Tuple _ st) <- getState
        when (st.visibility == On) $
          for_ (st.items !! index)
            \item -> H.raise (Selected item)

      CaptureRef event a -> a <$ do
        (Tuple _ st) <- getState
        let elementFromEvent
              = hush
              <<< runExcept
              <<< unsafeFromForeign
              <<< unsafeToForeign
              <<< currentTarget
        H.modify $ seeks _ { inputElement = elementFromEvent event }
        pure a

      Focus focusOrBlur a -> a <$ do
        (Tuple _ st) <- getState
        traverse_ (H.liftEffect <<< if focusOrBlur then focus else blur) st.inputElement

      Key ev a -> a <$ do
        setVis On
        let preventIt = H.liftEffect $ preventDefault $ KE.toEvent ev
        case KE.code ev of
          "ArrowUp"   -> preventIt *> eval' (highlight Prev)
          "ArrowDown" -> preventIt *> eval' (highlight Next)
          "Escape"    -> do
            (Tuple _ st) <- getState
            preventIt
            for_ st.inputElement (H.liftEffect <<< blur)
          "Enter"     -> do
            (Tuple _ st) <- getState
            preventIt
            for_ st.highlightedIndex (eval' <<< select)
          otherKey    -> pure unit

      PreventClick ev a -> a <$ do
        H.liftEffect <<< preventDefault <<< ME.toEvent $ ev

      SetVisibility v a -> a <$ do
        (Tuple _ st) <- getState
        when (st.visibility /= v) do
          H.modify $ seeks _ { visibility = v, highlightedIndex = Just 0 }
          H.raise $ VisibilityChanged v

      GetVisibility f -> do
        (Tuple _ st) <- getState
        pure (f st.visibility)

      ReplaceItems items a -> a <$ do
        H.modify $ seeks _
          { items = items
          , lastIndex = length items - 1
          , highlightedIndex = Nothing }

      Raise parentQuery a -> a <$ do
        H.raise (Emit parentQuery)

      Receive input a -> a <$ do
        H.modify (updateStore input.render identity)
