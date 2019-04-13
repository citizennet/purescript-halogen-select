module Docs.Components.Dropdown where

import Prelude

import Data.Const (Const)
import Effect.Aff (Aff)
import Data.Array ((!!), mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Docs.CSS as CSS
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Select as S
import Select.Setters as SS

type ExtraState =
  ( items :: Array String
  , selection :: Maybe String 
  )

data ExtraMessage
  = SelectionChanged (Maybe String) (Maybe String)

type Slot =
  H.Slot S.Query' (S.Message ExtraMessage)

spec :: S.Spec ExtraState (Const Void) Void () ExtraMessage Aff
spec = S.defaultSpec 
  { render = render
  , handleMessage = handleMessage 
  }
  where
  handleMessage = case _ of
    S.Selected ix -> do
      st <- H.get
      let selection = st.items !! ix
      H.modify_ _ { selection = selection }
      H.raise $ S.Message $ SelectionChanged st.selection selection
    _ -> pure unit

  render st = HH.div_ [ renderToggle, renderMenu ]
    where
    renderToggle =
      HH.button
        ( SS.setToggleProps st [ HP.classes CSS.button ] )
        [ HH.text $ fromMaybe "Select an option" st.selection ]

    renderMenu =
      HH.div 
        [ HP.classes CSS.menu ]
        ([ renderContainer renderItems ] # guard (st.visibility == S.Off))
      where
      renderContainer html =
        HH.div
          ( SS.setContainerProps [ HP.classes CSS.itemContainer ] )
          [ HH.ul [ HP.classes CSS.ul ] html ]

      renderItems =
        renderItem `mapWithIndex` st.items

      renderItem index item =
        HH.li 
          (SS.setItemProps index [ HP.class_ (HH.ClassName bgClass) ]) 
          [ HH.text item ]
        where
        bgClass = "bg-grey-lighter" # guard (st.highlightedIndex == Just index)

