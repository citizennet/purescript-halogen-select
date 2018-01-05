module Dropdown.Render where

import Prelude

import CSS as CSS
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Dropdown.Query (Query(..))
import Select.Dispatch (Dispatch, embed, getChildProps, getContainerProps, getItemProps, getToggleProps)
import Select.Primitive.Container as C

type DropdownItem = String

renderContainer :: (C.State String) -> H.HTML Void (Dispatch String Query)
renderContainer st =
  HH.div_
    $ if not st.open
      then [ renderToggle ]
      else [ renderToggle, renderItems $ renderItem `mapWithIndex` st.items ]
  where

    -- Render whatever is going to provide the action for toggling the menu. Notably, this is
    -- NOT a primitive.
    renderToggle :: H.HTML Void (Dispatch String Query)
    renderToggle =
      HH.span
      ( getToggleProps
        [ HE.onMouseOver $ HE.input_ $ embed (Log "I'm the parent.")
        , HP.class_      $ HH.ClassName "f5 link ba bw1 ph3 pv2 mb2 dib near-black pointer outline-0"
        ]
      )
        [ HH.text "Toggle" ]

    -- Render the container for the items
    renderItems :: Array (H.HTML Void (Dispatch DropdownItem Query))
                -> H.HTML Void (Dispatch String Query)
    renderItems html =
      HH.div
        ( getContainerProps
          [ HP.class_ $ HH.ClassName "measure ba br1 b--black-30 overflow-y-scroll pb3 outline-0"
          , HC.style $ CSS.maxHeight (CSS.px 300.0)
          ]
        )
        [ HH.div
            [ HP.class_ $ HH.ClassName "cf" ]
            [ HH.h4
                [ HP.class_ $ HH.ClassName "ph2 pv3 ma0 fl w-50" ]
                [ HH.text "Choose One" ]
            , HH.div
                [ HP.class_ $ HH.ClassName "fl w-50 tr" ]
                [ HH.button
                    ( getChildProps
                      [ HP.class_ $ HH.ClassName "ma2 ba bw1 ph3 pv2 dib b--near-black pointer outline-0 link"
                      , HE.onClick $ HE.input_ $ embed (Log "button in container clicked")
                      ]
                    )
                    [ HH.text "Click Me" ]
                ]
            ]
        , HH.ul
            [ HP.class_ $ HH.ClassName "list pl0 mt0 bt b--black-30" ]
            html
        ]

    renderItem :: Int -> DropdownItem -> H.HTML Void (Dispatch DropdownItem Query)
    renderItem index item = HH.li item' [ HH.text item ]
      where
        item' =
          getItemProps index
              [ HP.class_ $ HH.ClassName
                  $ "lh-copy pa2 ba bl-0 bt-0 br-0 b--dotted b--black-30"
                  <> if st.highlightedIndex == Just index then " bg-light-blue" else "" ]
