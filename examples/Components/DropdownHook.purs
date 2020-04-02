module Components.DropdownHook where

import Prelude

import Data.Array ((!!), mapWithIndex, length)
import Data.Const (Const)
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks (HookM, StateToken, useState)
import Halogen.Hooks as Hooks
import Internal.CSS (class_, classes_, whenElem)
import SelectHook (SelectState, useSelect)
import SelectHook as S

data Message
  = SelectionChanged (Maybe String) (Maybe String)

-- it is unnecessary to export your own input type, but doing so helps if you
-- would like to set some sensible defaults behind the scenes.
type Input =
  { items :: Array String
  , buttonLabel :: String
  }

component :: H.Component HH.HTML (Const Void) Input Message Aff
component = Hooks.component \{ items, buttonLabel } -> Hooks.do
  selection /\ tSelection <- useState Nothing
  select <- useSelect { inputType: S.Toggle
                      , search: Nothing
                      , debounceTime: Nothing
                      , getItemCount: pure (length items)
                      , handleEvent: handleEvent items tSelection
                      }

  let
    renderToggle =
      HH.button
        ( select.toggleProps <> [ class_ "Dropdown__toggle" ] )
        [ HH.text (fromMaybe buttonLabel selection) ]

    renderContainer = whenElem (select.state.visibility == S.On) \_ ->
      HH.div
        ( select.containerProps <> [ class_ "Dropdown__container" ] )
        ( renderItem `mapWithIndex` items )

    renderItem index item =
      HH.div
        ( (select.itemProps index) <>
            [ classes_
                [ "Dropdown__item"
                , "Dropdown__item--highlighted"
                    # guard (select.state.highlightedIndex == Just index)
                ]
            ]
        )
        [ HH.text item ]

  Hooks.pure $
    HH.div
      [ class_ "Dropdown" ]
      [ renderToggle, renderContainer ]
  where
  handleEvent
    :: Array String
    -> StateToken (Maybe String)
    -> StateToken SelectState
    -> S.Event
    -> HookM () Message Aff Unit
  handleEvent items tSelection tSelectState = case _ of
    S.Selected ix -> do
      selectState <- Hooks.get tSelectState
      oldSelection <- Hooks.get tSelection
      let newSelection = items !! ix
      Hooks.modify_ tSelectState (_ { visibility = S.Off })
      Hooks.put tSelection newSelection
      Hooks.raise $ SelectionChanged oldSelection newSelection
    _ -> pure unit
