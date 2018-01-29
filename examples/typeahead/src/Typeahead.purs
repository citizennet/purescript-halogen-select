module Typeahead where

import Prelude

import Control.Monad.Aff.Console (log)
import Data.Newtype
import CSS as CSS
import Data.Array (mapWithIndex)
import Data.Foldable (length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS as HC
import Select.Effects (FX)
import Select.Components.Typeahead as TA
import Select.Primitives.Container as C
import Select.Primitives.Search as S

newtype TypeaheadItem = TypeaheadItem { id :: Int, name :: String }
derive instance newtypeTypeaheadItem :: Newtype TypeaheadItem
derive instance eqTypeaheadItem :: Eq TypeaheadItem
instance stringComparableTypeaheadItem :: TA.StringComparable TypeaheadItem where
  toString = (_.name <<< unwrap)

data Query a
  = Log String a
  | HandleTypeahead (TA.TypeaheadMessage Query TypeaheadItem) a

type ChildQuery e = TA.TypeaheadQuery Query TypeaheadItem e
type ChildSlot = Unit

type HTML e = H.ParentHTML Query (ChildQuery e) ChildSlot (FX e)

type State =
  { items    :: Array TypeaheadItem
  , selected :: Array TypeaheadItem }

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

    render :: State -> HTML e
    render st =
      HH.div
        [ HP.class_ $ HH.ClassName "mw8 sans-serif center" ]
        [ HH.h2
          [ HP.class_ $ HH.ClassName "black-80 f-headline-1" ]
          [ HH.text "Typeahead Component"]
        , HH.slot
            unit
            TA.component
            { items: testData
            , debounceTime: Milliseconds 0.0
            , search: Nothing
            , initialSelection: TA.Many []
            , render: renderTypeahead
            , config: Right TA.defaultConfig
            }
            (HE.input HandleTypeahead)
        ]

    eval :: Query ~> H.ParentDSL State Query (ChildQuery e) ChildSlot Void (FX e)
    eval = case _ of
      Log str a -> a <$ do
        H.liftAff $ log str

      HandleTypeahead message a -> a <$ case message of
        TA.Emit query -> eval query
        TA.ItemSelected item -> H.liftAff $ log $ "Selected: " <> _.name item
        TA.ItemRemoved  item -> H.liftAff $ log $ "Removed: " <> _.name item


{-

Config

-}

testData :: Array TypeaheadItem
testData =
  [ { id: 0, name: "Thomas Honeyman" }
  , { id: 1, name: "Dave Zuch" }
  , { id: 2, name: "Chris Cornwell" }
  , { id: 3, name: "Forest Toney" }
  , { id: 4, name: "Lee Leathers" }
  , { id: 5, name: "Kim Wu" }
  , { id: 6, name: "Tara Strauss" }
  , { id: 7, name: "Sanket Sabnis" }
  , { id: 8, name: "Aaron Chu" }
  , { id: 9, name: "Vincent Busam" }
  , { id: 10, name: "Riley Gibbs" }
  , { id: 11, name: "Qian Liu" }
  ]

----------
-- Render Functions

-- Simple renderer for the typeahead. Render anything you want here,
-- but you're expected to mount the search and container slots.
renderTypeahead :: forall e
  . TA.TypeaheadState Query TypeaheadItem e
 -> TA.TypeaheadHTML Query TypeaheadItem e
renderTypeahead _ = HH.div_ [ TA.searchSlot searchPrim, TA.containerSlot containerPrim ]
  where
    searchPrim =
      { render: renderSearch
      , search: Nothing
      , debounceTime: Milliseconds 300.0 }

    containerPrim =
      { render: renderContainer
      , items: testData }


-- The user is using the Search primitive, so they have to fill out a Search render function
renderSearch :: ∀ e. (S.SearchState e) -> H.HTML Void (S.SearchQuery Query TypeaheadItem e)
renderSearch st =
  HH.input ( S.getInputProps [] )


-- The user is using the Container primitive, so they have to fill out a Container render function
renderContainer :: (C.ContainerState TypeaheadItem) -> H.HTML Void (C.ContainerQuery Query TypeaheadItem)
renderContainer st =
  HH.div_
    $ if not st.open
      then [ ]
      else [ renderItems $ renderItem `mapWithIndex` st.items ]
  where

    -- Render the container for the items
    renderItems :: Array (H.HTML Void (C.ContainerQuery Query TypeaheadItem))
                -> H.HTML Void (C.ContainerQuery Query TypeaheadItem)
    renderItems html =
      HH.div
        ( C.getContainerProps
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
                    ( C.getChildProps
                      [ HP.class_ $ HH.ClassName "ma2 ba bw1 ph3 pv2 dib b--near-black pointer outline-0 link"
                      , HE.onClick $ HE.input_ $ C.Raise $ H.action $ Log "I've been clicked!"  ]
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

    renderItem :: Int -> TypeaheadItem -> H.HTML Void (C.ContainerQuery Query TypeaheadItem)
    renderItem index item = HH.li item' [ HH.text (_.name item) ]
      where
        item' = C.getItemProps index
          [ HP.class_ $ HH.ClassName
            $ "lh-copy pa2 bb b--black-10"
            <> if st.highlightedIndex == Just index then " bg-light-blue" else "" ]
