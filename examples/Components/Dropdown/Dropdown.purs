module Docs.Components.Dropdown where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Data.Array as Array
import Data.Array ((!!))
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Docs.CSS as CSS
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Select as Select
import Select.Setters as Setters

{-
  There is no longer any need to write a new component to wrap Select. Instead, you can
  extend Select's state, input, query algebra (and by extension, actions), messages, and 
  child components. Rather than create a wrapping component, simply use Select with  
  your extensions in place!

  This module exports a version of Select extended to be a proper dropdown. It renders
  a button and maintains the current selection from the list.
-}

-----
-- Component

-- The types used to define this component are below; all we have to do is specialize
-- the type of the component for convenient exporting. 
-- 
-- It's trivial to swap out the render function, query handler, or message handler
-- for another, thereby building an entirely different component. In this case, we'll 
-- provide a render function and a handleMessage function, but use `pure Nothing` for 
-- our query handler as we don't need to extend the query algebra in any way.
component :: forall m. MonadAff m => H.Component HH.HTML Query Input Message m
component = Select.component render (\_ -> pure Nothing) handleMessage


-----
-- State

-- Select does not by default have any conception of 'selected' items, but we can 
-- extend its state to include a `selection` field which maintains the index of the 
-- selected item.
type ExtraState =
  ( selection :: Maybe Int )

-- We can create a new type synonym that extends Select's state with this new field.
type State = Select.State String ExtraState

-- We can do the same for the Input type if we want. We'll get the new fields in 
-- state via the input, so the parameter shows up here, too.
type Input = Select.Input String ExtraState


-----
-- Message

-- We don't need to add any new messages to Select, but we could if we wanted to!
type Message = Select.Message String Void

-- Sometimes you may need to take some action when a message is emitted. For example,
-- there are several ways an item can be selected in `Select` and you may want to
-- perform some action or query after this event has occurred. In these cases, you
-- can provide a handler for Select to manage these messages for itself in addition
-- to raising them to a parent.
handleMessage :: forall m. Message -> H.HalogenM State Action () Message m Unit
handleMessage = case _ of
  -- We want to take action when an item has been selected, so let's handle that.
  Select.Selected item -> do
    H.modify_ \st -> st { selection = Array.elemIndex item st.items }
  _ -> 
    pure unit


-----
-- Query

-- We're able to introduce all the new behavior we need via the `handleMessage` 
-- function, so we don't need to introduce any new queries to Select. However, you
-- can freely extend the query algebra (and, by extension, the actions) new queries 
-- so long as you also provide a handler for them.

-- For convenience, we'll create a synonym for `Query` with no extensions
type Query = Select.Query String (Const Void) ()

-- For convenience, we'll create a synonym for `Action` with no extensions.
type Action = Select.Action String (Const Void) ()


-----
-- Render Function

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div_ [ renderToggle, renderMenu ]
  where
  renderToggle =
    HH.button
      ( Setters.setToggleProps state [ HP.classes CSS.button ] )
      -- We can use our new `selection` value in state to display the selected item
      -- if there is one.
      [ HH.text $ fromMaybe "Select an option" $ (state.items !! _) =<< state.selection ]

  renderMenu =
    HH.div [ HP.classes CSS.menu ]
      if state.visibility == Select.Off
        then []
        else [ renderContainer $ renderItem `Array.mapWithIndex` state.items ]
    where
    renderContainer html =
      HH.div
        ( Setters.setContainerProps [ HP.classes CSS.itemContainer ] )
        [ HH.ul [ HP.classes CSS.ul ] html ]

    renderItem index item =
      HH.li
        ( Setters.setItemProps index props )
        [ HH.text item ]
      where
      props =
        [ HP.classes
          ( CSS.li <>
            if state.highlightedIndex == Just index
              then [ HH.ClassName "bg-grey-lighter" ]
              else []
          )
        ]

