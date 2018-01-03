module Typeahead where

import Prelude

import CSS as CSS
import Control.Monad.Aff.Console (log, logShow)
import Data.Array (difference, filter, mapWithIndex, (:))
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.Time.Duration (Milliseconds(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select.Dispatch (ContainerQuery(SetItems), Dispatch(..), getChildProps, getContainerProps, getInputProps, getItemProps, emit)
import Select.Dispatch as D
import Select.Effects (FX)
import Select.Primitive.Container as C
import Select.Primitive.Search as S

{-

This module contains an example typeahead component that uses the following primitives:

- Container
- Search

-}

-- aka 'a damn mess'
data Query a
  = HandleContainer (C.Message String Query) a
  | HandleSearch    (S.Message String Query)    a

data Slot = Slot Int
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type State =
  { items    :: Array (D.Item String)
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

    render :: State -> H.ParentHTML Query (Dispatch String Query) Slot (FX e)
    render st =
      HH.div
        [ HP.class_ $ HH.ClassName "mw8 sans-serif center" ]
        [ HH.h2
          [ HP.class_ $ HH.ClassName "black-80 f-headline-1" ]
          [ HH.text "Typeahead Component"]
        , HH.slot (Slot 0) (S.component renderSearch) { search: Nothing, debounceTime: Milliseconds 300.0 } (HE.input HandleSearch)
        , HH.slot (Slot 1) (C.component renderContainer) { items: testData } (HE.input HandleContainer)
        ]

    eval :: Query ~> H.ParentDSL State Query (Dispatch String Query) Slot Void (FX e)
    eval = case _ of
      HandleSearch m a -> case m of
        S.Emit q -> emit eval q a

        -- A new search is done: filter the results!
        S.NewSearch s -> a <$ do
          st <- H.get
          let filtered = filterItems s st.items
          let available = filterSelected filtered st.selected

          -- Allow insertion of elements
          let items
                | length available < 1 = D.Selectable s : available
                | otherwise            = available

          _ <- H.query (Slot 1)
                 $ H.action
                 $ Container
                 $ SetItems items
          pure a

      HandleContainer m a -> case m of
        C.Emit q -> emit eval q a

        C.ItemSelected item -> a <$ do
          st <- H.get
          if length (filter ((==) (D.Selectable item)) st.items) > 0
            then H.modify _ { selected = ( item : st.selected ) }
            else H.modify _ { items = ( (D.Selected item) : st.items ), selected = ( item : st.selected ) }

          st <- H.get
          _  <- H.query (Slot 1)
                  $ H.action
                  $ Container
                  $ SetItems
                  $ updateItems st.items st.selected

          H.liftAff $ log "List of selections..." *> logShow st.selected


{-

HELPERS

-}

filterItems :: String -> Array (D.Item String) -> Array (D.Item String)
filterItems str = filter (\i -> contains (Pattern str) (unpackItem i))

-- Brute force. Can be more elegant.
updateItems :: Array (D.Item String) -> Array String -> Array (D.Item String)
updateItems items selected = map (\i -> update i selected) items
  where
    -- If the item is in the selected list then update it
    update :: D.Item String -> Array String -> D.Item String
    update item arr = if length (filter (\m -> m == str) arr) > 0 then D.Selected str else item
      where
        str = unpackItem item

filterSelected :: Array (D.Item String) -> Array String -> Array (D.Item String)
filterSelected items selected = map (\i -> D.Selectable i) $ difference unpacked selected
  where
    unpacked = unpackItem <$> items


{-

CONFIGURATION

-}

-- The user is using the Search primitive, so they have to fill out a Search render function
renderSearch :: ∀ e. (S.State e) -> H.HTML Void (Dispatch String Query)
renderSearch st =
  HH.input ( getInputProps [] )

-- The user is using the Container primitive, so they have to fill out a Container render function
renderContainer :: (C.State String) -> H.HTML Void (Dispatch String Query)
renderContainer st =
  HH.div_
    $ if not st.open
      then [ ]
      else [ renderItems $ renderItem `mapWithIndex` st.items ]
  where

    -- Render the container for the items
    renderItems :: Array (H.HTML Void (Dispatch String Query))
                -> H.HTML Void (Dispatch String Query)
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

    renderItem :: Int -> D.Item String -> H.HTML Void (Dispatch String Query)
    renderItem index item = HH.li item' [ HH.text str ]
      where
        str :: String
        str = unpackItem item

        item' = case item of
          D.Selectable str -> getItemProps index
              [ HP.class_ $ HH.ClassName
                  $ "lh-copy pa2 bb b--black-10"
                  <> if st.highlightedIndex == Just index then " bg-light-blue" else "" ]

          D.Selected str -> getItemProps index
              [ HP.class_ $ HH.ClassName
                  $ "lh-copy pa2 bb b--black-10 bg-washed-blue"
                  <> if st.highlightedIndex == Just index then " bg-light-blue" else "" ]

          D.Disabled str -> getItemProps index
              [ HP.class_ $ HH.ClassName
                  $ "lh-copy pa2 bb black-20 b--black-10"
                  <> if st.highlightedIndex == Just index then " bg-moon-gray" else "" ]


unpackItem :: D.Item String -> String
unpackItem (D.Selected str)   = str
unpackItem (D.Selectable str) = str
unpackItem (D.Disabled str)   = str

-- The parent must provide some input data.
testData :: Array (D.Item String)
testData =
  [ D.Selectable "Thomas Honeyman"
  , D.Selectable "Dave Zuch"
  , D.Selectable "Chris Cornwell"
  , D.Disabled "Forest Toney"
  , D.Selectable "Lee Leathers"
  , D.Disabled "Kim Wu"
  , D.Selectable "Rachel Blair"
  , D.Selectable "Tara Strauss"
  , D.Selectable "Sanket Sabnis"
  , D.Selectable "Aaron Chu"
  , D.Selectable "Vincent Busam"
  , D.Selectable "Riley Gibbs"
  , D.Disabled "THE COOKIE MONSTER DID NOTHING WRONG" ]
