module Container where

import Prelude

import CSS as CSS
import Control.Monad.Aff.Console (log)
import DOM.Event.KeyboardEvent as KE
import Data.Array (difference, mapWithIndex, (:))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select.Menu as Menu
import Select.Utils (getToggleProps, getItemProps, inMenu)
import Select.Effects (FX)

{-

This module demonstrates a minimal parent component for a dropdown. To construct
a minimal example like this one, the end user will need to:

1. Add a handler for Menu.Message to their query algebra
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

-- 1. The parent must handle the Menu.Emit message with `eval q` and can
--    handle the Menu.Selected item message however they would like.
data Query a
  = NoOp a
  | Log String a
  | Handle (Menu.Message String Query) a

type State =
  { items    :: Array String
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

    render :: State -> H.ParentHTML Query (Menu.Query String Query) Unit (FX e)
    render st =
      HH.div
        [ HP.class_ $ HH.ClassName "mw8 sans-serif center" ]
        ( [ HH.h2
            [ HP.class_ $ HH.ClassName "black-80 f-headline-1"
            , HE.onKeyDown $ HE.input (Log <<< KE.code)
            ]
            [ HH.text "Menu Component"]
          , HH.p_
            [ HH.text "Open the console to view outputs. Mouse over the toggle to trigger an embedded parent query. Click the toggle to open or close the menu. Click an item to select it (and remove it from the available options)." ]

          -- The typeahead can be mounted anywhere
          , HH.slot unit (Menu.component renderMenu) { items: testData } (HE.input Handle) ]

          -- Selections are managed outside the component
          <> selected )
      where
        selected = if st.selected == [] then [] else
          [ HH.h2
              [ HP.class_ $ HH.ClassName "black-80 mt5 f-headline-1" ]
              [ HH.text "Selected Items"]
          , HH.ul_
              ( map (\i -> HH.li_ [ HH.text i ]) st.selected )
          ]

    -- Here, Menu.Emit recursively calls the parent eval function.
    -- Menu.Selected item is handled by removing that item from
    -- the options and maintaining it here in state.
    eval :: Query ~> H.ParentDSL State Query (Menu.Query String Query) Unit Void (FX e)
    eval = case _ of
      -- Dummy behavior to prove queries route back up properly
      NoOp a -> pure a

      Log s a -> a <$ do
        H.liftAff $ log s

      -- All child messages
      Handle m a -> case m of

        -- This is expected to call `eval q`
        Menu.Emit q -> do
          H.liftAff $ log "The child triggered a query..."
          eval q
          pure a

        -- The parent can do whatever they like here.
        Menu.Selected item -> do
          H.liftAff $ log ("Selected: " <> item)
          H.modify \st -> st { selected = ( item : st.selected ) }

          st <- H.get
          _  <- H.query unit
                  $ H.action
                  $ Menu.SetItems
                  $ difference st.items st.selected

          pure a


{-

CONFIGURATION

The parent will need to fill out this configuration information to function
correctly.

-}

-- The parent is mainly responsible for filling out a function with this type signature,
-- and attaching our queries. This can be done with our helper functions, or they can
-- attach everything as they see fit by hand.

renderMenu :: (Menu.State String) -> H.HTML Void (Menu.Query String Query)
renderMenu st =
  if not st.open
    then HH.div_ [ renderToggle ]
    else HH.div_ [ renderToggle, renderItems $ renderItem `mapWithIndex` st.items ]
  where
    -- Render whatever is going to provide the action for toggling the menu
    renderToggle :: H.HTML Void (Menu.Query String Query)
    renderToggle =
      HH.span
      ( getToggleProps
        [ HE.onMouseOver $ HE.input_ $ inMenu (Log "I'm the parent.")
        , HP.class_      $ HH.ClassName "f5 link ba bw1 ph3 pv2 mb2 dib near-black pointer outline-0"
        ]
      )
        [ HH.text "Toggle" ]

    -- Render the individual items
    renderItems :: Array (H.HTML Void (Menu.Query String Query))
                -> H.HTML Void (Menu.Query String Query)
    renderItems html =
      HH.ul
        [ HP.class_ $ HH.ClassName "list pl0 mt0 measure ba br1 b--black-30 overflow-y-scroll"
        , HC.style $ CSS.maxHeight (CSS.px 200.0) ]
        html

    renderItem :: Int -> String -> H.HTML Void (Menu.Query String Query)
    renderItem index item =
      HH.li
      ( getItemProps index
        [ HP.class_ $ HH.ClassName
            $ "lh-copy pa2 ba bl-0 bt-0 br-0 b--dotted b--black-30"
            <> if st.highlightedIndex == Just index then " bg-light-blue" else ""
        ]
      )
        [ HH.text item ]

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
  , "Riley Gibbs"
  , "THE COOKIE MONSTER DID NOTHING WRONG" ]
