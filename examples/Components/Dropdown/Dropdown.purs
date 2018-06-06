module Docs.Components.Dropdown where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Data.Array (difference, mapWithIndex)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select as Select
import Select.Utils.Setters as Setters
import Docs.CSS as CSS

type State =
  { items :: Array String
  , text :: String }

type Input =
  { items :: Array String }

data Query a
  = HandleSelect (Select.Message Query String) a

data Message
  = Void

type ChildSlot = Unit
type ChildQuery = Select.Query Query String

component :: ∀ m. MonadAff m => H.Component HH.HTML Query Input Message m
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: Input -> State
    initialState i = { items: i.items, text: "Select an option" }

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message m
    eval = case _ of
      HandleSelect (Select.Selected item) a -> do
        st <- H.get
        _ <- H.query unit $ Select.setVisibility Select.Off
        _ <- H.query unit $ Select.replaceItems $ difference st.items [ item ]
        H.modify_ _ { text = item }
        pure a

      HandleSelect other a -> pure a

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
    render st =
      HH.div
        [ HP.class_ $ HH.ClassName "w-full" ]
        [ HH.slot unit Select.component input (HE.input HandleSelect) ]
      where
        input =
          { initialSearch: Nothing
          , debounceTime: Nothing
          , inputType: Select.Toggle
          , items: difference st.items [ st.text ]
          , render: renderDropdown
          }

        renderDropdown :: Select.State String -> Select.ComponentHTML Query String
        renderDropdown state = HH.div_ [ renderToggle, renderMenu ]
          where
            renderToggle =
              HH.button
                ( Setters.setToggleProps [ HP.classes CSS.button ] )
                [ HH.text st.text ]

            renderMenu =
              HH.div [ HP.classes CSS.menu ]
              $ if state.visibility == Select.Off
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

