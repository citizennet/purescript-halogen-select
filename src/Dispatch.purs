module Select.Dispatch where

import Prelude

import Control.Comonad
import Control.Comonad.Store
import Control.Monad.Aff (Fiber)
import Control.Monad.Aff.AVar (AVar)
import Data.Identity (Identity(..))
import Data.Maybe
import Data.Time.Duration (Milliseconds)
import Data.Tuple
import DOM.Event.KeyboardEvent (KeyboardEvent)
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select.Effects (Effects)

{-

This module contains the query types for all primitives and allows multiple components
to share query types.

-}

-- Here's the idea: Create a single `Dispatch` type that serves as the query type for ALL
-- primitives. In any given primitive, case on your specific query type, and return all
-- others wrapped in Emit.

data Dispatch item o e a
  = ParentQuery (o Unit) a
  | Search (SearchQuery item o e) a
  | Container (ContainerQuery item o e) a


{-

SEARCH PRIMITIVE

-}

-- Primitive query types using `a` as a phantom argument to allow being used as an action
data SearchQuery item o e
  = TextInput String
  | SearchReceiver (SearchInput item o e)

type SearchState e =
  { search    :: String
  , ms        :: Milliseconds
  , debouncer :: Maybe (Debouncer e)
  }

type Debouncer e =
  { var   :: AVar String
  , fiber :: Fiber (Effects e) Unit }

type SearchInput item o e =
  { search :: Maybe String
  , debounceTime :: Milliseconds
  , render :: SearchState e -> H.ComponentHTML (Dispatch item o e) }


{-

CONTAINER PRIMITIVE

-}

data ContainerQuery item o e
  = Highlight   Target              -- Change the highlighted item
  | Select      Int                 -- Select a particular item
  | Key         KeyboardEvent       -- A key has been pressed
  | Mouse       MouseState          -- Update mousedown state
  | Blur                            -- Blur event
  | Visibility  VisibilityStatus    -- Open or close the menu
  | ContainerReceiver (ContainerInput item o e)

data Target
  = Next
  | Prev
  | Index Int

data MouseState
  = Down
  | Up

data VisibilityStatus
  = On
  | Off
  | Toggle

type ContainerState item =
  { items            :: Array item
  , open             :: Boolean
  , highlightedIndex :: Maybe Int
  , lastIndex        :: Int
  , mouseDown        :: Boolean
  }

type ContainerInput item o e =
  { items  :: Array item
  , render :: ContainerState item -> H.ComponentHTML (Dispatch item o e) }


emit :: ∀ a0 a1 o item e f. Applicative f => (o Unit -> f Unit) -> Dispatch item o e a0 -> a1 -> f a1
emit f (ParentQuery o _) a = a <$ f o
emit _ _ a = pure a

updateStore :: ∀ state html. (state -> html) -> (state -> state) -> Store state html -> Store state html
updateStore r f = (\(Tuple _ s) -> store r s) <<< runStore <<< seeks f


--
--
-- RENDERING
--
--

augmentHTML :: forall t q q' -- q q' represents parent query wrapped by child query
  . Array (H.IProp t (q q')) -- Our query type
 -> Array (H.IProp t (q q')) -- User query
 -> Array (H.IProp t (q q'))
augmentHTML = flip (<>)

-- Embed a parent query
embed :: ∀ item parentQuery e. H.Action parentQuery -> Unit -> Dispatch item parentQuery e Unit
embed = ParentQuery <<< H.action

-- Intended for use on the text input field.
getInputProps = augmentHTML
  [ HE.onFocus      $ HE.input_ $ Container $ Visibility Toggle
  , HE.onKeyDown    $ HE.input  $ \ev -> Container $ Key ev
  , HE.onValueInput $ HE.input  $ \ev -> Search $ TextInput ev
  , HE.onMouseDown  $ HE.input_ $ Container $ Mouse Down
  , HE.onMouseUp    $ HE.input_ $ Container $ Mouse Up
  , HE.onBlur       $ HE.input_ $ Container $ Blur
  , HP.tabIndex 0
  ]

-- Intended for use on a clickable toggle
getToggleProps = augmentHTML
  [ HE.onClick      $ HE.input_ $ Container $ Visibility Toggle
  , HE.onKeyDown    $ HE.input  $ \ev -> Container $ Key ev
  , HE.onMouseDown  $ HE.input_ $ Container $ Mouse Down
  , HE.onMouseUp    $ HE.input_ $ Container $ Mouse Up
  , HE.onBlur       $ HE.input_ $ Container $ Blur
  , HP.tabIndex 0
  ]

-- Intended to be used on the container primitive itself
getContainerProps = augmentHTML
  [ HE.onMouseDown $ HE.input_ $ Container $ Mouse Down
  , HE.onMouseUp   $ HE.input_ $ Container $ Mouse Up
  , HE.onBlur      $ HE.input_ $ Container $ Blur
  , HP.tabIndex 0
  ]

-- -- Intended for anything that will be embedded into the container primitive
getChildProps = augmentHTML
  [ HE.onBlur      $ HE.input_ $ Container $ Blur
  , HP.tabIndex 0
  ]

getItemProps index = augmentHTML
  [ HE.onClick     $ HE.input_ $ Container $ Select index
  , HE.onMouseOver $ HE.input_ $ Container $ Highlight (Index index)
  , HE.onKeyDown   $ HE.input  $ \ev -> Container $ Key ev
  , HE.onBlur      $ HE.input_ $ Container $ Blur
  , HP.tabIndex 0
  ]
