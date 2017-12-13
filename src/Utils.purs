module Select.Utils where

import Prelude

import Halogen as H

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
  . Array (H.IProp t (q q')) -- Our properties
 -> Array (H.IProp t (q q')) -- User properties
 -> Array (H.IProp t (q q'))
augmentHTML = flip (<>)
