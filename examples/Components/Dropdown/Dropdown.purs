module Docs.Components.Dropdown where

import Prelude

import Data.Array (difference, mapWithIndex)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..), onMatch)
import Docs.CSS as CSS
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Select as Select
import Select.Setters as Setters

type State =
  { items :: Array String
  , text :: String 
  }

type Input =
  { items :: Array String 
  }

data Action
  = HandleSelect (Select.Message String ())

type Query = Const Void
type Message = Void

type ChildSlots = 
  ( select :: H.Slot (Select.Query String () ()) (Select.Message String ()) Unit
  )

_select = SProxy :: SProxy "select"

component :: ∀ m. MonadAff m => H.Component HH.HTML Query Input Message m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  initialState :: Input -> State
  initialState i = { items: i.items, text: "Select an option" }

  handleAction :: Action -> H.HalogenM State Action ChildSlots Message m Unit
  handleAction = case _ of
    HandleSelect msg -> onMatch
      { selected: \item -> do
          st <- H.get
          _ <- H.query _select unit $ H.tell $ Select.PerformAction $ Select.setVisibility Select.Off
          _ <- H.query _select unit $ H.tell $ Select.ReplaceItems $ difference st.items [ item ]
          H.modify_ _ { text = item }
      } (\_ -> pure unit) msg

  render :: State -> H.ComponentHTML Action ChildSlots m
  render st =
    HH.div
      [ HP.class_ $ HH.ClassName "w-full" ]
      [ HH.slot _select unit (Select.component renderDropdown (\_ -> pure unit)) input (Just <<< HandleSelect) ]
    where
    input =
      { search: Nothing
      , debounceTime: Nothing
      , inputType: Select.Toggle
      , items: difference st.items [ st.text ]
      }

    renderDropdown :: Select.State String () -> H.ComponentHTML (Select.Action ()) () m
    renderDropdown state = HH.div_ [ renderToggle, renderMenu ]
      where
      renderToggle =
        HH.button
          ( Setters.setToggleProps state [ HP.classes CSS.button ] )
          [ HH.text st.text ]

      renderMenu =
        HH.div [ HP.classes CSS.menu ]
          if state.visibility == Select.Off
            then []
            else [ renderContainer $ renderItem `mapWithIndex` state.items ]
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

