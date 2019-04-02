-- | This module exposes a component that can be used to build accessible selection
-- | user interfaces. You are responsible for providing all rendering, with the help
-- | of the `Select.Setters` module, but this component provides the relevant
-- | behaviors for dropdowns, autocompletes, typeaheads, keyboard-navigable calendars,
-- | and other selection UIs.
module Select where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Data.Array (length, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_, traverse, traverse_)
import Effect.Aff (Fiber, delay, error, forkAff, killFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Renderless.State (getState, modifyState_, modifyStore)
import Web.Event.Event (preventDefault)
import Web.HTML.HTMLElement (blur, focus)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

-----
-- Component Types

-- | A useful shorthand for the Halogen component type
type Component o item m = H.Component HH.HTML (Query o item) (Input o item) (Message o item) m

-- | A useful shorthand for the Halogen component HTML type
type ComponentHTML o item = H.ComponentHTML (Query o item)

-- | A useful shorthand for the Halogen component DSL type
type ComponentDSL o item m = H.ComponentDSL (StateStore o item) (Query o item) (Message o item) m

-- | The component's state type, wrapped in `Store`. The state and result of the
-- | render function are stored so that `extract` from `Control.Comonad` can be
-- | used to pull out the render function.
type StateStore o item = Store (State item) (ComponentHTML o item)

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
data Query o item a
  = Search String a
  | Highlight Target a
  | Select Int a
  | Focus Boolean a
  | Key KE.KeyboardEvent a
  | PreventClick ME.MouseEvent a
  | SetVisibility Visibility a
  | GetVisibility (Visibility -> a)
  | ReplaceItems (Array item) a
  | Raise (o Unit) a
  | Initialize a
  | Receive (Input o item) a
  | AndThen (Query o item Unit) (Query o item Unit) a

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
-- | - `debounceRef`: A representation of a running timer that, when it expires, will
-- |                  trigger debounced events.
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
  , debounceRef      :: Maybe (Ref (Maybe Debouncer))
  , items            :: Array item
  , visibility       :: Visibility
  , highlightedIndex :: Maybe Int
  , lastIndex        :: Int
  }

-- | Represents a running computation that, when it completes, will trigger debounced
-- | .cts.
type Debouncer =
  { var   :: AVar Unit
  , fiber :: Fiber Unit 
  }

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

component :: ∀ o item m. MonadAff m => Component o item m
component =
  H.lifecycleComponent
    { initialState
    , render: extract
    , eval
    , receiver: \i -> Just (Receive i unit) 
    , initializer: Just (Initialize unit)
    , finalizer: Nothing
    }
  where
    initialState i = store i.render
      { inputType: i.inputType
      , search: fromMaybe "" i.initialSearch
      , debounceTime: fromMaybe (Milliseconds 0.0) i.debounceTime
      , debounceRef: Nothing
      , items: i.items
      , highlightedIndex: Nothing
      , visibility: Off
      , lastIndex: length i.items - 1
      }

    -- Just the normal Halogen eval
    eval :: Query o item ~> ComponentDSL o item m
    eval = case _ of
      Initialize a -> a <$ do
        ref <- H.liftEffect $ Ref.new Nothing
        modifyState_ _ { debounceRef = Just ref }

      Search str a -> a <$ do
        st <- getState
        ref :: Maybe Debouncer <- H.liftEffect $ map join $ traverse Ref.read st.debounceRef
        modifyState_ _ { search = str }
        void $ H.fork $ eval $ SetVisibility On a

        case st.inputType, ref of
          TextInput, Nothing -> unit <$ do
            var   <- H.liftAff AVar.empty
            fiber <- H.liftAff $ forkAff do
              delay st.debounceTime
              AVar.put unit var

            -- This compututation will fork and run in the background. When the
            -- var is finally filled, the action will run (raise a new search)
            _ <- H.fork do
              _ <- H.liftAff $ AVar.take var
              void $ H.liftEffect $ traverse_ (Ref.write Nothing) st.debounceRef
              modifyState_ _ { highlightedIndex = Just 0 }
              newState <- getState
              H.raise $ Searched newState.search

            void $ H.liftEffect $ traverse_ (Ref.write $ Just { var, fiber }) st.debounceRef

          TextInput, Just debouncer -> do
            let var = debouncer.var
            _ <- H.liftAff $ killFiber (error "Time's up!") debouncer.fiber
            fiber <- H.liftAff $ forkAff do
              delay st.debounceTime
              AVar.put unit var

            void $ H.liftEffect $ traverse_ (Ref.write $ Just { var, fiber }) st.debounceRef

          -- Key stream is not yet implemented. However, this should capture user
          -- key events and expire their search after a set number of milliseconds.
          _, _ -> pure unit

      Highlight target a -> a <$ do
        st <- getState
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
          modifyState_ _ { highlightedIndex = highlightedIndex }

      Select index a -> a <$ do
        st <- getState
        when (st.visibility == On) $
          for_ (st.items !! index)
            \item -> H.raise (Selected item)

      Focus focusOrBlur a -> a <$ do
        inputElement <- H.getHTMLElementRef $ H.RefLabel "select-input"
        traverse_ (H.liftEffect <<< if focusOrBlur then focus else blur) inputElement

      Key ev a -> a <$ do
        void $ H.fork $ eval $ H.action $ SetVisibility On
        let preventIt = H.liftEffect $ preventDefault $ KE.toEvent ev
        case KE.code ev of
          "ArrowUp"   -> preventIt *> eval (Highlight Prev unit)
          "ArrowDown" -> preventIt *> eval (Highlight Next unit)
          "Escape"    -> do
            inputElement <- H.getHTMLElementRef $ H.RefLabel "select-input"
            preventIt
            for_ inputElement (H.liftEffect <<< blur)
          "Enter"     -> do
            st <- getState
            preventIt
            for_ st.highlightedIndex \x -> eval $ Select x a
          otherKey    -> pure unit

      PreventClick ev a -> a <$ do
        H.liftEffect <<< preventDefault <<< ME.toEvent $ ev

      SetVisibility v a -> a <$ do
        st <- getState
        when (st.visibility /= v) do
          modifyState_ _ { visibility = v, highlightedIndex = Just 0 }
          H.raise $ VisibilityChanged v

      GetVisibility f -> do
        st <- getState
        pure (f st.visibility)

      ReplaceItems items a -> a <$ do
        modifyState_ _
          { items = items
          , lastIndex = length items - 1
          , highlightedIndex = Nothing 
          }

      Raise parentQuery a -> a <$ do
        H.raise (Emit parentQuery)

      Receive input a -> a <$ do
        modifyStore input.render identity
      
      AndThen q1 q2 a -> eval q1 *> eval q2 *> pure a
