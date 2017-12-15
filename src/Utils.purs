module Select.Utils where

import Prelude

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select.Menu as Menu
import Select.Typeahead as Typeahead

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
inMenu :: ∀ item t f. (Unit -> t Unit) -> f -> Menu.Query item t f
inMenu = Menu.ParentQuery <<< H.action

inTypeahead :: ∀ item t f. (Unit -> t Unit) -> f -> Typeahead.Query item t f
inTypeahead = Typeahead.ParentQuery <<< H.action

getInputProps = augmentHTML
  [ HE.onFocus      $ HE.input_ $ Menu.Visibility Menu.Toggle
  , HE.onKeyDown    $ HE.input  $ Menu.Key
  -- , HE.onValueInput $ HE.input  Typeahead.Search
  , HP.tabIndex 0
  ]

getToggleProps = augmentHTML
  [ HE.onClick     $ HE.input_ $ Menu.Visibility Menu.Toggle
  , HE.onKeyDown   $ HE.input  $ Menu.Key
-- , HE.onBlur      $ HE.input_ $ Menu.Visibility Menu.Off  -- can be re-added if wanted
  , HP.tabIndex 0
  ]

getItemProps index = augmentHTML
  [ HE.onMouseDown $ HE.input_ $ Menu.Select index -- onMouseDown allows for onBlur in event ordering
  , HE.onMouseOver $ HE.input_ $ Menu.Highlight (Menu.Index index)
  , HE.onKeyDown   $ HE.input  $ Menu.Key
  , HP.tabIndex 0
  ]
