module Select.Utils where

import Prelude

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select.Primitive.Container as Container

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
-- properly.
inContainer :: ∀ item t f. (Unit -> t Unit) -> f -> Container.Query item t f
inContainer = Container.ParentQuery <<< H.action

getInputProps = augmentHTML
  [ HE.onFocus     $ HE.input_ $ Container.Visibility Container.Toggle
  , HE.onKeyDown   $ HE.input  $ Container.Key
  , HE.onMouseDown $ HE.input_ $ Container.Mouse Container.Down
  , HE.onMouseUp   $ HE.input_ $ Container.Mouse Container.Up
  , HE.onBlur      $ HE.input_ $ Container.Blur
  , HP.tabIndex 0
  ]

getToggleProps = augmentHTML
  [ HE.onClick     $ HE.input_ $ Container.Visibility Container.Toggle
  , HE.onKeyDown   $ HE.input  $ Container.Key
  , HE.onMouseDown $ HE.input_ $ Container.Mouse Container.Down
  , HE.onMouseUp   $ HE.input_ $ Container.Mouse Container.Up
  , HE.onBlur      $ HE.input_ $ Container.Blur
  , HP.tabIndex 0
  ]

getContainerProps = augmentHTML
  [ HE.onMouseDown $ HE.input_ $ Container.Mouse Container.Down
  , HE.onMouseUp   $ HE.input_ $ Container.Mouse Container.Up
  , HE.onBlur      $ HE.input_ $ Container.Blur
  , HP.tabIndex 0
  ]

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
