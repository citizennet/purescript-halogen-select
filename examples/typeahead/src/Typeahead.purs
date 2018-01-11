module Typeahead where

import Prelude

import Control.Monad.Aff.Console (log, logShow)
import CSS as CSS
import Data.Array (mapWithIndex, difference, filter, (:))
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.Time.Duration (Milliseconds(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS as HC
import Select.Dispatch (SearchState, ContainerState, ContainerQuery(..), Dispatch(..), emit, getInputProps, getContainerProps, getChildProps, getItemProps)
import Select.Effects (FX)
import Select.Primitive.Container as C
import Select.Primitive.Search as S

{-

-}

type TypeaheadItem = String

data Query e a
  = HandleContainer (C.Message String (Query e) e) a
  | HandleSearch    (S.Message String (Query e) e) a

data Slot = Slot Int
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type State =
  { items    :: Array TypeaheadItem
  , selected :: Array TypeaheadItem }

component :: ∀ e. H.Component HH.HTML (Query e) Unit Void (FX e)
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

    render :: State -> H.ParentHTML (Query e) (Dispatch TypeaheadItem (Query e) e) Slot (FX e)
    render st =
      HH.div
        [ HP.class_ $ HH.ClassName "mw8 sans-serif center" ]
        [ HH.h2
          [ HP.class_ $ HH.ClassName "black-80 f-headline-1" ]
          [ HH.text "Typeahead Component"]
        , HH.slot (Slot 0) S.component { render: renderSearch, search: Nothing, debounceTime: Milliseconds 300.0 } ( HE.input HandleSearch )
        , HH.slot (Slot 1) C.component { render: renderContainer, items: testData } ( HE.input HandleContainer )
        ]

    eval :: (Query e) ~> H.ParentDSL State (Query e) (Dispatch TypeaheadItem (Query e) e) Slot Void (FX e)
    eval = case _ of
      HandleSearch m a -> case m of
        -- Can't use EMIT here because the search toggle sends events to the container
        S.Emit q -> case q of
          -- A container event has occurred, so dispatch that to the container slot
          Container c _ -> do
            _ <- H.query (Slot 1)
                   $ H.action
                   $ Container c
            pure a

          ParentQuery p _ -> a <$ eval p

          -- Search won't emit itself, so it can be ignored safely.
          _ -> pure a

        -- A new search is done: filter the results!
        S.NewSearch s -> a <$ do
          st <- H.get

          H.liftAff $ log $ "New search performed: " <> s

          let filtered  = filterItems s st.items
          let available = difference filtered st.selected

          -- Allow insertion of elements
          let newItems
                | length available < 1 = s : available
                | otherwise            = available

          _ <- H.query (Slot 1)
                 $ H.action
                 $ Container
                 $ ContainerReceiver { render: renderContainer, items: newItems }
          pure a

      HandleContainer m a -> case m of
        C.Emit q -> emit eval q a

        C.ItemSelected item -> a <$ do
          st <- H.get
          if length (filter ((==) item) st.items) > 0
            then H.modify _ { selected = ( item : st.selected ) }
            else H.modify _ { items = ( item : st.items ), selected = ( item : st.selected ) }

          newSt <- H.get
          _  <- H.query (Slot 1)
                  $ H.action
                  $ Container
                  $ ContainerReceiver { render: renderContainer, items: difference newSt.items newSt.selected }

          H.liftAff $ log "List of selections..." *> logShow newSt.selected


{-

HELPERS

-}

filterItems :: TypeaheadItem -> Array TypeaheadItem -> Array TypeaheadItem
filterItems str = filter (\i -> contains (Pattern str) i)

{-

Config

-}

testData :: Array TypeaheadItem
testData =
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
  , "THE COOKIE MONSTER DID NOTHING WRONG"
  ]


-- Render Functions
-- The user is using the Search primitive, so they have to fill out a Search render function
renderSearch :: ∀ i o e. (SearchState e) -> H.HTML Void (Dispatch i o e)
renderSearch st =
  HH.input ( getInputProps [] )

-- The user is using the Container primitive, so they have to fill out a Container render function
renderContainer :: ∀ o e. (ContainerState TypeaheadItem) -> H.HTML Void (Dispatch TypeaheadItem o e)
renderContainer st =
  HH.div_
    $ if not st.open
      then [ ]
      else [ renderItems $ renderItem `mapWithIndex` st.items ]
  where

    -- Render the container for the items
    renderItems :: Array (H.HTML Void (Dispatch TypeaheadItem o e))
                -> H.HTML Void (Dispatch TypeaheadItem o e)
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

    renderItem :: Int -> TypeaheadItem -> H.HTML Void (Dispatch TypeaheadItem o e)
    renderItem index item = HH.li item' [ HH.text item ]
      where
        item' =
          getItemProps index
            [ HP.class_ $ HH.ClassName
              $ "lh-copy pa2 bb b--black-10"
              <> if st.highlightedIndex == Just index then " bg-light-blue" else "" ]
