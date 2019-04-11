module Docs.Components.Dropdown where

import Prelude

import Data.Const (Const)
import Effect.Aff.Class (class MonadAff)
import Data.Array ((!!), mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Docs.CSS as CSS
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Select as Select
import Select.Setters as Setters

type ExtraState =
  ( items :: Array String
  , selection :: Maybe String 
  )

data ExtraMessage
  = SelectionChanged (Maybe String) (Maybe String)

type Slot =
  H.Slot Select.Query' (Select.Message ExtraMessage)

spec :: forall m. MonadAff m => Select.Spec ExtraState (Const Void) () ExtraMessage m 
spec = Select.defaultSpec 
  { render = render
  , handleMessage = handleMessage 
  }
  where
  handleMessage = case _ of
    Select.Selected ix -> do
      st <- H.get
      let selection = st.items !! ix
      H.modify_ _ { selection = selection }
      H.raise $ Select.Raised $ SelectionChanged st.selection selection
    _ -> pure unit

  render state = HH.div_ [ renderToggle, renderMenu ]
    where
    renderToggle =
      HH.button
        ( Setters.setToggleProps state [ HP.classes CSS.button ] )
        [ HH.text $ fromMaybe "Select an option" state.selection ]

    renderMenu =
      HH.div 
        [ HP.classes CSS.menu ]
        ([ renderContainer $ renderItem `mapWithIndex` state.items ] # guard (state.visibility == Select.Off))
      where
      renderContainer html =
        HH.div
          ( Setters.setContainerProps [ HP.classes CSS.itemContainer ] )
          [ HH.ul [ HP.classes CSS.ul ] html ]

      renderItem index item =
        HH.li
          ( Setters.setItemProps index [ HP.class_ bgClass ] )
          [ HH.text item ]
        where
        bgClass = HH.ClassName $ "bg-grey-lighter" # guard (state.highlightedIndex == Just index)

