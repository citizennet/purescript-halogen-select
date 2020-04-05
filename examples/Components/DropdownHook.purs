module Components.DropdownHook where

import Prelude

import Data.Array ((!!), mapWithIndex, length)
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks (useState)
import Halogen.Hooks as Hooks
import Internal.CSS (class_, classes_, whenElem)
import Example.Hooks.UseEvent (subscribeTo)
import SelectHook (useSelect)
import SelectHook as S

type Slot query = H.Slot query Message

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
                      }
  subscribeTo select.onSelectedIdxChanged \ix -> do
      oldSelection <- Hooks.get tSelection
      let newSelection = items !! ix
      select.setVisibility S.Off
      Hooks.put tSelection newSelection
      Hooks.raise $ SelectionChanged oldSelection newSelection

  let
    renderToggle =
      HH.button
        ( select.toggleProps <> [ class_ "Dropdown__toggle" ] )
        [ HH.text (fromMaybe buttonLabel selection) ]

    renderContainer = whenElem (select.visibility == S.On) \_ ->
      HH.div
        ( select.containerProps <> [ class_ "Dropdown__container" ] )
        ( renderItem `mapWithIndex` items )

    renderItem index item =
      HH.div
        ( (select.itemProps index) <>
            [ classes_
                [ "Dropdown__item"
                , "Dropdown__item--highlighted"
                    # guard (select.highlightedIndex == Just index)
                ]
            ]
        )
        [ HH.text item ]

  Hooks.pure $
    HH.div
      [ class_ "Dropdown" ]
      [ renderToggle, renderContainer ]
