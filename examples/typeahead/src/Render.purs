module Typeahead.Render where

import Prelude

import CSS as CSS
import Data.Array (mapWithIndex)
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Select.Primitive.Container as C
import Select.Primitive.Search as S
import Select.Dispatch (Dispatch, getChildProps, getContainerProps, getInputProps, getItemProps)

type TypeaheadItem = String

-- The user is using the Search primitive, so they have to fill out a Search render function
renderSearch :: ∀ i o e. (S.State e) -> H.HTML Void (Dispatch i o)
renderSearch st =
  HH.input ( getInputProps [] )

-- The user is using the Container primitive, so they have to fill out a Container render function
renderContainer :: ∀ o. (C.State TypeaheadItem) -> H.HTML Void (Dispatch TypeaheadItem o)
renderContainer st =
  HH.div_
    $ if not st.open
      then [ ]
      else [ renderItems $ renderItem `mapWithIndex` st.items ]
  where

    -- Render the container for the items
    renderItems :: Array (H.HTML Void (Dispatch TypeaheadItem o))
                -> H.HTML Void (Dispatch TypeaheadItem o)
    renderItems html =
      HH.div
        ( getContainerProps
          [ HP.class_ $ HH.ClassName "measure ba br1 b--black-30 overflow-y-scroll outline-0"
          , HC.style $ CSS.maxHeight (CSS.px 300.0)
          ]
        )
        ([ HH.div
            [ HP.class_ $ HH.ClassName "cf" ]
            [ HH.h4
                [ HP.class_ $ HH.ClassName "ph2 pv3 ma0 fl w-50" ]
                [ HH.text "Choose One" ]
            , HH.div
                [ HP.class_ $ HH.ClassName "fl w-50 tr" ]
                [ HH.button
                    ( getChildProps
                      [ HP.class_ $ HH.ClassName "ma2 ba bw1 ph3 pv2 dib b--near-black pointer outline-0 link" ]
                    )
                    [ HH.text "Click Me" ]
                ]
            ]
         ]
         <> if length html > 0
             then
               [ HH.ul
                 [ HP.class_ $ HH.ClassName "list pa0 ma0 bt b--black-30" ]
                 html ]
             else
               [ HH.p [HP.class_ $ HH.ClassName "lh-copy black-70 pa2"] [ HH.text "No results for that search." ] ]
        )

    renderItem :: Int -> TypeaheadItem -> H.HTML Void (Dispatch TypeaheadItem o)
    renderItem index item = HH.li item' [ HH.text item ]
      where
        item' =
          getItemProps index
            [ HP.class_ $ HH.ClassName
              $ "lh-copy pa2 bb b--black-10"
              <> if st.highlightedIndex == Just index then " bg-light-blue" else "" ]
