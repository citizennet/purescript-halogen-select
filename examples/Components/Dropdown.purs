module Components.Dropdown where

import Prelude

import Data.Const (Const)
import Effect.Aff (Aff)
import Data.Array ((!!), mapWithIndex, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Halogen as H
import Halogen.HTML as HH
import Internal.CSS (class_, classes_, whenElem)
import Select as S
import Select.Setters as SS

type Slot =
  H.Slot S.Query' Message

type State =
  ( items :: Array String
  , selection :: Maybe String
  , buttonLabel :: String
  )

data Message
  = SelectionChanged (Maybe String) (Maybe String)

-- it is unnecessary to export your own input type, but doing so helps if you
-- would like to set some sensible defaults behind the scenes.
type Input =
  { items :: Array String
  , buttonLabel :: String
  }

component :: H.Component HH.HTML S.Query' Input Message Aff
component = S.component input $ S.defaultSpec
    { render = render
    , handleMessage = handleMessage
    }
  where
    input :: Input -> S.Input State
    input { items, buttonLabel } =
      { inputType: S.Toggle
      , search: Nothing
      , debounceTime: Nothing
      , getItemCount: length <<< _.items
      , items
      , buttonLabel
      , selection: Nothing
      }

    handleMessage :: S.Message -> H.HalogenM (S.State State) S.Action' Message Aff Unit
    handleMessage = case _ of
      S.Selected ix -> do
        st <- H.get
        let selection = st.items !! ix
        H.modify_ _ { selection = selection, visibility = S.Off }
        H.raise $ SelectionChanged st.selection selection
      _ -> pure unit

    render :: S.State State -> H.ComponentHTML S.Action' () Aff
    render st =
      HH.div
        [ class_ "Dropdown" ]
        [ renderToggle, renderContainer ]
      where
      renderToggle =
        HH.button
          ( SS.setToggleProps [ class_ "Dropdown__toggle" ] )
          [ HH.text (fromMaybe st.buttonLabel st.selection) ]

      renderContainer = whenElem (st.visibility == S.On) \_ ->
        HH.div
          ( SS.setContainerProps [ class_ "Dropdown__container" ] )
          ( renderItem `mapWithIndex` st.items )
        where
        renderItem index item =
          HH.div
            ( SS.setItemProps index
                [ classes_
                    [ "Dropdown__item"
                    , "Dropdown__item--highlighted" # guard (st.highlightedIndex == Just index)
                    ]
                ]
            )
            [ HH.text item ]
