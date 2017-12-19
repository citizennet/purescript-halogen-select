module Select.Utils where

import Prelude

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select.Primitive.Container as Container
import Select.Primitive.Search as Search

{-

This module exposes shared building blocks for each component.

-}

-- The base function for extending a user-provided IProp with those necessary
-- for our layer to function. This is meant to be partially-applied with our properties
-- and then provided to the user as helper functions. For that reason, we use `flip`
-- here so our properties are applied last -- this ensures that in the case of duplication
-- ours overwrite theirs.
--
-- TODO: Allow multiple event handlers for one event source.
augmentHTML :: forall t q q' -- q q' represents parent query wrapped by child query
  . Array (H.IProp t (q q')) -- Our query type
 -> Array (H.IProp t (q q')) -- User query
 -> Array (H.IProp t (q q'))
augmentHTML = flip (<>)


--
-- RENDER HELPERS

-- A convenience for the parent to ensure they embed their queries
-- into primitives properly.
inContainer :: ∀ item parent. H.Action parent -> Unit -> Container.Query item parent Unit
inContainer = Container.ParentQuery <<< H.action
-- inContainer :: ∀ item parent. H.Action parent -> Unit -> Unit -> Wrap item parent Unit
-- inContainer q = C <<< Container.ParentQuery (H.action q)

inSearch :: ∀ parent. H.Action parent -> Unit -> Search.Query parent Unit
inSearch = Search.ParentQuery <<< H.action
-- inSearch :: ∀ item parent. H.Action parent -> Unit -> Unit -> Wrap item parent Unit
-- inSearch q = S <<< Search.ParentQuery (H.action q)

-- data Wrap item o a
--   = C (Container.Query item o Unit) a
--   | S (Search.Query o Unit) a

-- Intended for use on the text input field.
-- getInputProps = augmentHTML
--   [ HE.onFocus      $ HE.input_ $ C $ Container.Visibility Container.Toggle unit
--   , HE.onKeyDown    $ HE.input  $ \ev -> C $ Container.Key ev unit
--   , HE.onValueInput $ HE.input  $ \ev -> S $ Search.TextInput ev unit
--   , HE.onMouseDown  $ HE.input_ $ C $ Container.Mouse Container.Down unit
--   , HE.onMouseUp    $ HE.input_ $ C $ Container.Mouse Container.Up unit
--   , HE.onBlur       $ HE.input_ $ C $ Container.Blur unit
--   , HP.tabIndex 0
--   ]

getInputProps = augmentHTML
  [ HE.onFocus      $ HE.input_ $ Container.Visibility Container.Toggle
  , HE.onKeyDown    $ HE.input  $ Container.Key
  -- , HE.onValueInput $ HE.input  $ Search.TextInput
  , HE.onMouseDown  $ HE.input_ $ Container.Mouse Container.Down
  , HE.onMouseUp    $ HE.input_ $ Container.Mouse Container.Up
  , HE.onBlur       $ HE.input_ $ Container.Blur
  , HP.tabIndex 0
  ]

-- Intended for a toggle button that will affect the container component
getToggleProps = augmentHTML
  [ HE.onClick     $ HE.input_ $ Container.Visibility Container.Toggle
  , HE.onKeyDown   $ HE.input  $ Container.Key
  , HE.onMouseDown $ HE.input_ $ Container.Mouse Container.Down
  , HE.onMouseUp   $ HE.input_ $ Container.Mouse Container.Up
  , HE.onBlur      $ HE.input_ $ Container.Blur
  , HP.tabIndex 0
  ]

-- Intended to be used on the container primitive itself
getContainerProps = augmentHTML
  [ HE.onMouseDown $ HE.input_ $ Container.Mouse Container.Down
  , HE.onMouseUp   $ HE.input_ $ Container.Mouse Container.Up
  , HE.onBlur      $ HE.input_ $ Container.Blur
  , HP.tabIndex 0
  ]

-- Intended for anything that will be embedded into the container primitive
getChildProps = augmentHTML
  [ HE.onBlur      $ HE.input_ $ Container.Blur
  , HP.tabIndex 0
  ]

getItemProps index = augmentHTML
  [ HE.onClick     $ HE.input_ $ Container.Select index
  , HE.onMouseOver $ HE.input_ $ Container.Highlight (Container.Index index)
  , HE.onKeyDown   $ HE.input  $ Container.Key
  , HE.onBlur      $ HE.input_ $ Container.Blur
  , HP.tabIndex 0
  ]
