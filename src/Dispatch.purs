module Select.Dispatch where

import Prelude

import DOM.Event.KeyboardEvent (KeyboardEvent)
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

{-

This module contains the query types for all primitives and allows multiple components
to share query types.

-}

-- Here's the idea: Create a single `Dispatch` type that serves as the query type for ALL
-- primitives. In any given primitive, case on your specific query type, and return all
-- others wrapped in Emit.

data Dispatch item o a
  = ParentQuery (o Unit) a
  | S (SearchQuery Unit) a
  | C (ContainerQuery item Unit) a


-- Primitive query types using `a` as a phantom argument to allow being used as an action
data SearchQuery a
  = TextInput String

data ContainerQuery item a
  = Highlight   Target              -- Change the highlighted item
  | Select      Int                 -- Select a particular item
  | Key         KeyboardEvent       -- A key has been pressed
  | Mouse       MouseState          -- Update mousedown state
  | Blur                            -- Blur event
  | Visibility  VisibilityStatus    -- Open or close the menu
  | SetItems    (Array (Item item)) -- Set the data (used by parent)

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

data Item item
  = Selected item
  | Selectable item
  | Disabled item

instance showItem :: Show item => Show (Item item) where
  show (Selected item)   = "Selected: " <> show item
  show (Selectable item) = "Selectable: " <> show item
  show (Disabled item)   = "Disabled: " <> show item

instance eqItem :: Eq item => Eq (Item item) where
  eq (Selected a) (Selected b) = eq a b
  eq (Selectable a) (Selectable b) = eq a b
  eq (Disabled a) (Disabled b) = eq a b
  eq (Selected a) _ = false
  eq (Selectable a) _ = false
  eq (Disabled a) _ = false
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
embed :: ∀ item parentQuery. H.Action parentQuery -> Unit -> Dispatch item parentQuery Unit
embed = ParentQuery <<< H.action

-- Intended for use on the text input field.
getInputProps = augmentHTML
  [ HE.onFocus      $ HE.input_ $ C $ Visibility Toggle
  , HE.onKeyDown    $ HE.input  $ \ev -> C $ Key ev
  , HE.onValueInput $ HE.input  $ \ev -> S $ TextInput ev
  , HE.onMouseDown  $ HE.input_ $ C $ Mouse Down
  , HE.onMouseUp    $ HE.input_ $ C $ Mouse Up
  , HE.onBlur       $ HE.input_ $ C $ Blur
  , HP.tabIndex 0
  ]


-- Intended for use on a clickable toggle
getToggleProps = augmentHTML
  [ HE.onClick      $ HE.input_ $ C $ Visibility Toggle
  , HE.onKeyDown    $ HE.input  $ \ev -> C $ Key ev
  , HE.onMouseDown  $ HE.input_ $ C $ Mouse Down
  , HE.onMouseUp    $ HE.input_ $ C $ Mouse Up
  , HE.onBlur       $ HE.input_ $ C $ Blur
  , HP.tabIndex 0
  ]

-- Intended to be used on the container primitive itself
getContainerProps = augmentHTML
  [ HE.onMouseDown $ HE.input_ $ C $ Mouse Down
  , HE.onMouseUp   $ HE.input_ $ C $ Mouse Up
  , HE.onBlur      $ HE.input_ $ C $ Blur
  , HP.tabIndex 0
  ]

-- -- Intended for anything that will be embedded into the container primitive
getChildProps = augmentHTML
  [ HE.onBlur      $ HE.input_ $ C $ Blur
  , HP.tabIndex 0
  ]

getItemProps index = augmentHTML
  [ HE.onClick     $ HE.input_ $ C $ Select index
  , HE.onMouseOver $ HE.input_ $ C $ Highlight (Index index)
  , HE.onKeyDown   $ HE.input  $ \ev -> C $ Key ev
  , HE.onBlur      $ HE.input_ $ C $ Blur
  , HP.tabIndex 0
  ]
