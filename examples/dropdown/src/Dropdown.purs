module Dropdown where

import Prelude
import Dropdown.Render (renderContainer)
import Dropdown.Query

import Control.Monad.Aff.Console (log, logShow)
import DOM.Event.KeyboardEvent as KE
import Data.Array (filter, (:), difference)
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select.Dispatch (ContainerQuery(SetItems), Dispatch(Container), emit)
import Select.Dispatch as D
import Select.Effects (FX)
import Select.Primitive.Container as C

{-

This module demonstrates a minimal parent component for a dropdown. To construct
a minimal example like this one, the end user will need to:

1. Add a handler for Message to their query algebra
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

type State =
  { items    :: Array DropdownItem
  , selected :: Array DropdownItem }

type DropdownItem = String

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

    render :: State -> H.ParentHTML Query (Dispatch DropdownItem Query) Unit (FX e)
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
          , HH.slot unit (C.component renderContainer) { items: testData } (HE.input HandleContainer) ]

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
    eval :: Query ~> H.ParentDSL State Query (Dispatch DropdownItem Query) Unit Void (FX e)
    eval = case _ of
      Log s a -> a <$ do
        H.liftAff $ log s

      HandleContainer m a -> case m of
        C.Emit q -> emit eval q a

        C.ItemSelected item -> do
          H.liftAff $ log ("Selected: " <> item)
          H.modify \st -> st { selected = ( item : st.selected ) }

          st <- H.get
          H.liftAff $ logShow st.selected
          _  <- H.query unit
                  $ H.action
                  $ Container
                  $ SetItems
                  $ difference st.items st.selected
          pure a


{-

CONFIGURATION

-}


-- The parent must provide some input data.
testData :: Array DropdownItem
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
