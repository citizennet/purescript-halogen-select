module Select.Typeahead where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Select.Effects (FX)

{-

The typeahead builds on the dropdown to incorporate more behavior.

-}

-- It is possible to embed the dropdown's queries here and selectively route them downward,
-- thus presenting a single interface to the end user. But that couples them together; instead
-- I have taken the approach of only selectively adding properties on top.
data Query item o a
  = ParentQuery (o Unit) a   -- Return an embedded query to the parent
  | Search String a

type State item =
  { contents :: Maybe String }

type Input item = String

-- All components must allow for emitting the parent's queries back up to the parent.
data Message item o
  = Emit (o Unit)    -- duplicated from menu component so the parent doesn't have to unwrap
  | Searched String  -- the new behavior: searches

-- The component is responsible for behaviors but defers the render completely to the parent.
-- That render function can be written with the help of our `getProps` helpers, or you can
-- write it yourself completely.
component :: ∀ item o e
   . (State item -> H.ComponentHTML (Query item o))
  -> H.Component HH.HTML (Query item o) (Input item) (Message item o) (FX e)
component render =
  H.component
    { initialState: \i -> { contents: if i == "" then Nothing else Just i }
    , render
    , eval
    , receiver: const Nothing
    }
  where
    eval :: (Query item o) ~> H.ComponentDSL (State item) (Query item o) (Message item o) (FX e)
    eval = case _ of
      ParentQuery o a -> a <$ do
        H.raise $ Emit o

      Search s a -> a <$ do
        H.raise $ Searched s
