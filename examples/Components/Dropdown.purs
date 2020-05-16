module Components.Dropdown where

import Prelude

import Data.Array ((!!), mapWithIndex, length)
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks (useLifecycleEffect, useState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks (useEvent)
import Internal.CSS (class_, classes_, whenElem)
import Select (SelectReturn(..), selectInput, useSelect)
import Select as S

type Slot = H.Slot (Const Void) Message

data Message
  = SelectionChanged (Maybe String) (Maybe String)

-- it is unnecessary to export your own input type, but doing so helps if you
-- would like to set some sensible defaults behind the scenes.
type Input =
  { items :: Array String
  , buttonLabel :: String
  }

component :: H.Component HH.HTML (Const Void) Input Message Aff
component = Hooks.component \tokens { items, buttonLabel } -> Hooks.do
  selection /\ selectionId <- useState Nothing
  selectedIndexChanges <- useEvent
  SelectReturn select <- useSelect $ selectInput
    { getItemCount = pure (length items)
    , pushSelectedIdxChanged = selectedIndexChanges.push
    }

  useLifecycleEffect do
    void $ selectedIndexChanges.setCallback $ Just \_ ix -> do
      oldSelection <- Hooks.get selectionId
      let newSelection = items !! ix
      select.setVisibility S.Off
      Hooks.put selectionId newSelection
      Hooks.raise tokens.outputToken $ SelectionChanged oldSelection newSelection

    pure Nothing

  Hooks.pure $
    HH.div
      [ class_ "Dropdown" ]
      [ renderToggle select buttonLabel selection
      , renderContainer select items
      ]
  where
    renderToggle select buttonLabel selection =
      HH.button
        ( select.setToggleProps [ class_ "Dropdown__toggle" ] )
        [ HH.text (fromMaybe buttonLabel selection) ]

    renderContainer select items =
      whenElem (select.visibility == S.On) \_ ->
        HH.div
          ( select.setContainerProps [ class_ "Dropdown__container" ] )
          ( mapWithIndex (renderItem select) items )

    renderItem select index item =
      HH.div
        ( select.setItemProps index
            [ classes_
                [ "Dropdown__item"
                , "Dropdown__item--highlighted"
                    # guard (select.highlightedIndex == Just index)
                ]
            ]
        )
        [ HH.text item ]
