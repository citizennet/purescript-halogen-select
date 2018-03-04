module Select where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, seeks, store)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff (Fiber, delay, error, forkAff, killFiber)
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, putVar, takeVar, AVAR)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.Event (preventDefault, currentTarget)
import DOM.Event.KeyboardEvent as KE
import DOM.Event.FocusEvent as FE
import DOM.HTML.HTMLElement (focus)
import DOM.HTML.Types (HTMLElement, readHTMLElement)
import Data.Array (length, (!!))
import Data.Either (hush)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Halogen (Component, ComponentDSL, ComponentHTML, component, liftAff, liftEff, modify) as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.HalogenM (fork, raise) as H
import Select.Internal.State (updateStore, getState)

----------
-- Component Types

type Component o item eff m
  = H.Component HH.HTML (Query o item eff) (Input o item eff) (Message o item) m

type ComponentHTML o item eff
  = H.ComponentHTML (Query o item eff)

type ComponentDSL o item eff m
  = H.ComponentDSL (StateStore o item eff) (Query o item eff) (Message o item) m

type StateStore o item eff
  = Store (State item eff) (ComponentHTML o item eff)

type Effects eff = ( avar :: AVAR, dom :: DOM | eff )


----------
-- Core Constructors

data Query o item eff a
  = Search String a
  | TriggerFocus a
  | CaptureFocus FE.FocusEvent a
  | Highlight Target a
  | Select Int a
  | Key KE.KeyboardEvent a
  | Mouse MouseState a
  | Blur a
  | SetVisibility Visibility a
  | ToggleVisibility a
  | ReplaceItems (Array item) a
  | Raise (o Unit) a
  | Receive (Input o item eff) a

data Target = Prev | Next | Index Int
derive instance eqTarget :: Eq Target

data MouseState = Down | Up
derive instance eqMouseState :: Eq MouseState

data Visibility = Off | On
derive instance eqVisibility :: Eq Visibility

data InputType
  = TextInput
  | Toggle

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
  , mouseDown        :: Boolean
  }

type Debouncer eff =
  { var   :: AVar Unit
  , fiber :: Fiber eff Unit }

type Input o item eff =
  { inputType     :: InputType
  , items         :: Array item
  , initialSearch :: Maybe String
  , debounceTime  :: Maybe Milliseconds
  , render        :: State item eff -> ComponentHTML o item eff
  }

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
      , mouseDown: false
      }

    eval :: (Query o item (Effects eff)) ~> ComponentDSL o item (Effects eff) m
    eval = case _ of
      Search str a -> a <$ do
        (Tuple _ st) <- getState
        H.modify $ seeks _ { search = str }

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
              H.modify $ seeks _ { debouncer = Nothing }
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

      TriggerFocus a -> a <$ do
        (Tuple _ st) <- getState
        traverse_ (H.liftEff <<< focus) st.inputElement

      CaptureFocus focusEvent a -> a <$ do
        (Tuple _ st) <- getState
        let elementFromFocusEvent
              = hush
              <<< runExcept
              <<< readHTMLElement
              <<< toForeign
              <<< currentTarget
              <<< FE.focusEventToEvent

        H.modify $ seeks _ { inputElement = elementFromFocusEvent focusEvent }
        eval $ SetVisibility On a

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
          Just item -> do
             H.raise (Selected item)
             eval (TriggerFocus a)
          _ -> pure a -- Should not be possible.

      Key keyEvent a -> do
        (Tuple _ st) <- getState
        if st.visibility == Off then pure a else do
           H.liftEff $ preventDefault $ KE.keyboardEventToEvent keyEvent
           case KE.code keyEvent of
            "ArrowUp"   -> eval $ Highlight Prev a
            "ArrowDown" -> eval $ Highlight Next a
            "Enter"     -> a <$ traverse_ (\index -> eval $ Select index a) st.highlightedIndex
            "Escape"    -> eval $ SetVisibility Off a
            otherKey    -> pure a

      Mouse m a -> do
        (Tuple _ st) <- getState
        if st.visibility == Off then pure a else case m of
          Down -> H.modify (seeks _ { mouseDown = true })  *> pure a
          Up   -> H.modify (seeks _ { mouseDown = false }) *> eval (TriggerFocus a)

      Blur a -> do
        (Tuple _ st) <- getState
        if st.visibility == Off || st.mouseDown
          then pure a
          else eval $ SetVisibility Off a

      SetVisibility v a -> a <$ do
        case v of
          Off -> H.modify $ seeks _ { visibility = v }
          On  -> H.modify $ seeks _ { visibility = v, highlightedIndex = Nothing }
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

      Raise parentQuery a -> a <$ H.raise (Emit parentQuery)

      Receive input a -> a <$ H.modify (updateStore input.render id)

