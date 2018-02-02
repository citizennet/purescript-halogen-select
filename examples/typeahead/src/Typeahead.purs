module Typeahead where

import Prelude

import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (log, logShow)
import CSS as CSS
import Data.Array (mapWithIndex, difference, filter, (:))
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.Time.Duration (Milliseconds(..))
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Halogen.Component.ChildPath as CP
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS as HC
import Select.Primitives.Container as C
import Select.Primitives.Search as S
import Select.Effects (Effects)

type TypeaheadItem = String
type TypeaheadEffects e = ( now :: NOW | Effects e )

data Query a
  = Log String a
  | HandleContainer (C.Message Query TypeaheadItem) a
  | HandleSearch    (S.Message Query TypeaheadItem) a

type ChildQuery e = Coproduct2 (C.ContainerQuery Query TypeaheadItem) (S.SearchQuery Query TypeaheadItem (TypeaheadEffects e))
type ChildSlot = Either2 Unit Unit

type State =
  { items    :: Array TypeaheadItem
  , selected :: Array TypeaheadItem }

component :: ∀ m e
  . MonadAff ( TypeaheadEffects e ) m
 => H.Component HH.HTML Query Unit Void m
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

    render :: State -> H.ParentHTML Query (ChildQuery e) ChildSlot m
    render st =
      HH.div
        [ HP.class_ $ HH.ClassName "mw8 sans-serif center" ]
        [ HH.h2
          [ HP.class_ $ HH.ClassName "black-80 f-headline-1" ]
          [ HH.text "Typeahead Component"]
        , HH.slot'
            CP.cp2
            unit
            S.component
            { render: renderSearch, search: Nothing, debounceTime: Milliseconds 300.0 }
            ( HE.input HandleSearch )
        , HH.slot'
            CP.cp1
            unit
            C.component
            { render: renderContainer, items: testData }
            ( HE.input HandleContainer )
        ]

    eval :: Query ~> H.ParentDSL State Query (ChildQuery e) ChildSlot Void m
    eval = case _ of
      Log str a -> a <$ do
        H.liftAff $ log str

      HandleSearch m a -> case m of
        S.ContainerQuery q -> do
          _ <- H.query' CP.cp1 unit q
          pure a

        S.Emit q -> eval q *> pure a

        -- A new search is done: filter the results!
        S.NewSearch s -> a <$ do
          st <- H.get

          x <- H.liftEff now
          H.liftAff $ log $ "New search performed: " <> s
          H.liftAff $ log $ "Time: " <> show x

          let filtered  = filterItems s st.items
          let available = difference filtered st.selected

          -- Allow insertion of elements
          let newItems
                | length available < 1 = s : available
                | otherwise            = available

          _ <- H.query' CP.cp1 unit
                 $ H.action
                 $ C.ContainerReceiver { render: renderContainer, items: newItems }
          pure a

      HandleContainer m a -> case m of
        C.Emit q -> eval q *> pure a

        C.ItemSelected item -> a <$ do
          st <- H.get
          if length (filter ((==) item) st.items) > 0
            then H.modify _ { selected = ( item : st.selected ) }
            else H.modify _ { items = ( item : st.items ), selected = ( item : st.selected ) }

          newSt <- H.get
          _  <- H.query' CP.cp1 unit
                  $ H.action
                  $ C.ContainerReceiver { render: renderContainer, items: difference newSt.items newSt.selected }

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
  ]


-- Render Functions
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
    renderItem index item = HH.li item' [ HH.text item ]
      where
        item' = C.getItemProps index
          [ HP.class_ $ HH.ClassName
            $ "lh-copy pa2 bb b--black-10"
            <> if st.highlightedIndex == Just index then " bg-light-blue" else "" ]
