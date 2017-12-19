module Typeahead where

import Prelude
import CSS as CSS
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Monad.Aff.Console (log, logShow)
import Data.Array (filter, mapWithIndex, (:))
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Select.Effects (FX)
import Select.Primitive.Container as Container
import Select.Primitive.Search as Search
import Select.Utils (getChildProps, getContainerProps, getItemProps, getToggleProps)

{-

This module contains an example typeahead component that uses the following primitives:

- Container
- Search

-}

-- 1. The parent must handle the Menu.Emit message with `eval q` and can
--    handle the Menu.Selected item message however they would like.
data Query a
  = HandleContainer (Container.Message String Query) a
  | HandleSearch (Search.Message Query) a

type State =
  { items    :: Array (Container.Item String)
  , selected :: Array String }

component :: ∀ e. H.Component HH.HTML Query Unit Void (FX e)
component =
  H.parentComponent
    { initialState: const initState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initState :: State
    initState = { items: testData, selected: [] }

    render :: State -> H.ParentHTML Query (Container.Query String Query) Unit (FX e)
    render st =
      HH.div
        [ HP.class_ $ HH.ClassName "mw8 sans-serif center" ]
        [ HH.h2
          [ HP.class_ $ HH.ClassName "black-80 f-headline-1" ]
          [ HH.text "Typeahead Component"]

          -- The typeahead can be mounted anywhere
        , HH.slot unit (Container.component renderContainer) { items: testData } (HE.input HandleContainer)
        ]

    -- Here, Menu.Emit recursively calls the parent eval function.
    eval :: Query ~> H.ParentDSL State Query (Container.Query String Query) Unit Void (FX e)
    eval = case _ of
      HandleSearch m a -> a <$ case m of
        Search.Emit q -> eval q
        Search.NewSearch s -> H.liftAff $ log ("Searched: " <> s)

      HandleContainer m a -> a <$ case m of
        -- This is expected to call `eval q`
        Container.Emit q -> eval q

        -- The parent can do whatever they like here.
        Container.ItemSelected item -> do
          H.liftAff $ log ("Selected: " <> item)
          H.modify \st -> st { selected = ( item : st.selected ) }

          st <- H.get
          _  <- H.query unit
                  $ H.action
                  $ Container.SetItems
                  $ updateItems st.items st.selected
          H.liftAff $ logShow st.selected


{-

HELPERS

-}

-- Brute force. Can be more elegant.
updateItems :: Array (Container.Item String) -> Array String -> Array (Container.Item String)
updateItems items selected = map (\i -> update i selected) items
  where
    -- If the item is in the selected list then updated it
    update :: Container.Item String -> Array String -> Container.Item String
    update item arr = if length (filter (\m -> m == str) arr) > 0 then Container.Selected str else item
      where
        str = getStr item

{-

CONFIGURATION

The parent will need to fill out this configuration information to function
correctly.

-}

-- The parent is mainly responsible for filling out a function with this type signature,
-- and attaching our queries. This can be done with our helper functions, or they can
-- attach everything as they see fit by hand.

renderContainer :: (Container.State String) -> H.HTML Void (Container.Query String Query)
renderContainer st =
  HH.div_
    $ if not st.open
      then [ renderToggle ]
      else [ renderToggle, renderItems $ renderItem `mapWithIndex` st.items ]
  where

    -- Render whatever is going to provide the action for toggling the menu. Notably, this is
    -- NOT a primitive.
    renderToggle :: H.HTML Void (Container.Query String Query)
    renderToggle =
      HH.span
      ( getToggleProps
        [ HP.class_ $ HH.ClassName "f5 link ba bw1 ph3 pv2 mb2 dib near-black pointer outline-0" ]
      )
        [ HH.text "Toggle" ]

    -- Render the container for the items
    renderItems :: Array (H.HTML Void (Container.Query String Query))
                -> H.HTML Void (Container.Query String Query)
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
                      [ HP.class_ $ HH.ClassName "ma2 ba bw1 ph3 pv2 dib b--near-black pointer outline-0 link" ]
                    )
                    [ HH.text "Click Me" ]
                ]
            ]
        , HH.ul
            [ HP.class_ $ HH.ClassName "list pl0 mt0 bt b--black-30" ]
            html
        ]

    renderItem :: Int -> Container.Item String -> H.HTML Void (Container.Query String Query)
    renderItem index item = HH.li item' [ HH.text str ]
      where
        str :: String
        str = getStr item

        item' = case item of
          Container.Selectable str -> getItemProps index
              [ HP.class_ $ HH.ClassName
                  $ "lh-copy pa2 ba bl-0 bt-0 br-0 b--dotted b--black-30"
                  <> if st.highlightedIndex == Just index then " bg-light-blue" else "" ]

          Container.Selected str -> getItemProps index
              [ HP.class_ $ HH.ClassName
                  $ "lh-copy pa2 ba bl-0 bt-0 br-0 b--dotted b--black-30 bg-washed-blue"
                  <> if st.highlightedIndex == Just index then " bg-light-blue" else "" ]

          Container.Disabled str ->
              [ HP.class_ $ HH.ClassName
                  $ "lh-copy pa2 ba bl-0 bt-0 br-0 b--dotted black-30 b--black-30"
                  <> if st.highlightedIndex == Just index then " bg-light-gray" else "" ]


getStr :: Container.Item String -> String
getStr (Container.Selected str)   = str
getStr (Container.Selectable str) = str
getStr (Container.Disabled str)   = str

-- The parent must provide some input data.
testData :: Array (Container.Item String)
testData = map (\i -> Container.Selectable i)
  [ "Thomas Honeyman"
  , "Dave Zuch"
  , "Chris Cornwell"
  , "Forest Toney"
  , "Lee Leathers"
  , "Kim Wu"
  , "Rachel Blair"
  , "Tara Strauss"
  , "Sanket Sabnis"
  , "Aaron Chu"
  , "Vincent Busam"
  , "Riley Gibbs"
  , "THE COOKIE MONSTER DID NOTHING WRONG" ]
