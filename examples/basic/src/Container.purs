module Container where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import DOM (DOM)
import Data.Array (difference, (:))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)
import Select.Dropdown as Dropdown

{-

This module demonstrates a minimal parent component for a dropdown. To construct
a minimal example like this one, the end user will need to:

1. Add a handler for Dropdown.Message to their query algebra
   1a. Add a handler for the Emit message, which should simply recursively call
       `eval q`; this routes the query back into the parent's query algebra.
   1b. Add a handler for the Selected item message; this behavior is up to the
       parent to handle as the developer sees fit.

2. Prepare the rendering functions necessary for the dropdown component, using
   the helpers exported by that module.

3. Prepare input data to provide to the component.

4. Initialize the dropdown component in a new slot
   4a. If there already exist other types in slots, extend the coproduct to
       include the dropdown.

-}

type FX e = Aff (Effects e)
type Effects e = ( dom :: DOM, console :: CONSOLE, ajax :: AJAX | e)

-- 1. The parent must handle the Dropdown.Emit message with `eval q` and can
--    handle the Dropdown.Selected item message however they would like.
data Query a
  = NoOp a
  | Handle (Dropdown.Message String Query) a

type State =
  { items :: Array String
  , selected :: Array String }


component :: ∀ e. H.Component HH.HTML Query Unit Void (FX e)
component =
  H.parentComponent
    { initialState: const initState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initState :: State
    initState = { items: testData, selected: [] }

    render :: State -> H.ParentHTML Query (Dropdown.Query String Query) Unit (FX e)
    render st =
      HH.div
        [ HP.class_ $ HH.ClassName "mw8 sans-serif center" ]
        ( [ HH.h2
            [ HP.class_ $ HH.ClassName "black-80 f-headline-1" ]
            [ HH.text "Dropdown Component"]

          , HH.p_
            [ HH.text "Open the console to view outputs. Mouse over the toggle to trigger an embedded parent query. Click the toggle to open or close the menu. Click an item to select it (and remove it from the available options)." ]

          -- The typeahead can be mounted anywhere
          , HH.slot unit Dropdown.component dropdownInput (HE.input Handle) ]

          -- Selections are managed outside the component
          <> selected )
      where
        selected = if st.selected == [] then [] else
          [ HH.h2
              [ HP.class_ $ HH.ClassName "black-80 mt5 f-headline-1" ]
              [ HH.text "Selected Items"]
          , HH.ul_ $ map (\i -> HH.li_ [ HH.text i ]) st.selected ]

    -- Here, Dropdown.Emit recursively calls the parent eval function.
    -- Dropdown.Selected item is handled by removing that item from
    -- the options and maintaining it here in state.
    eval :: Query ~> H.ParentDSL State Query (Dropdown.Query String Query) Unit Void (FX e)
    eval = case _ of
      -- Dummy behavior to prove queries route back up properly
      NoOp a -> a <$ do
        H.liftAff $
          log "The child routed this query back to the parent."

      -- All child messages
      Handle m a -> case m of

        -- This is expected to call `eval q`
        Dropdown.Emit q -> do
          H.liftAff $ log "The child triggered a query..."
          eval q
          pure a

        -- The parent can do whatever they like here.
        Dropdown.Selected item -> do
          H.liftAff $ log ("Selected: " <> item)
          H.modify \st -> st { selected = (item : st.selected) }

          st <- H.get
          _  <- H.query unit
                  $ H.action
                  $ Dropdown.SetItems
                  $ filterItems st.items st.selected
          pure a


{-

HELPERS

The parent is responsible for managing selected items, so they might want
to add their own filtering.

-}

-- This serves simply to remove items present in both lists
filterItems :: Array String -> Array String -> Array String
filterItems = difference


{-

CONFIGURATION

The parent will need to fill out this configuration information to function
correctly.

-}

-- The parent is responsible for constructing this configuration
-- record, which includes the two render functions and the items.
dropdownInput :: Dropdown.Input String Query
dropdownInput =
  { render: { toggle: renderToggle, item: renderItem }
  , items: testData }

-- Render whatever is going to provide the action for toggling the menu
renderToggle :: H.HTML Void (Dropdown.Query String Query)
renderToggle =
  HH.span
    -- The user can embed their own queries with `embedQuery`; they'll be triggered
    -- with the Emit message.
    ( Dropdown.getToggleProps
       [ HE.onMouseOver $ HE.input_ $ Dropdown.embedQuery NoOp
       , HP.class_ $ HH.ClassName "f5 link ba bw1 ph3 pv2 mb2 dib near-black pointer" ]
    )
    [ HH.text "Toggle" ]

-- Render the individual items
renderItem :: String -> H.HTML Void (Dropdown.Query String Query)
renderItem str =
  HH.li
    -- The user doesn't have to provide any properties.
    ( Dropdown.getItemProps str
       [ HP.class_ $ HH.ClassName "lh-copy pv2 ba bl-0 bt-0 br-0 b--dotted b--black-30 hover-bg-light-blue" ]
    )
    [ HH.text str ]

-- The parent must provide some input data.
testData :: Array String
testData =
  [ "Thomas Honeyman"
  , "Dave Zuch"
  , "Chris Cornwell"
  , "Forest Toney"
  , "Lee Leathers"
  , "Kim Wu"
  , "Rachel Blair"
  , "Tara Strauss"
  , "Sanket Sabnis"
  , "Aaron Chu"
  , "Vincent Busam"
  , "Riley Gibbs" ]
