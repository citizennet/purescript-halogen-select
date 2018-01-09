module Typeahead where

import Prelude
import Typeahead.Render (renderContainer, renderSearch)

import Control.Monad.Aff.Console (log, logShow)
import Data.Array (difference, filter, (:))
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.Time.Duration (Milliseconds(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select.Dispatch (ContainerQuery(..), Dispatch(Container, ParentQuery), emit)
import Select.Dispatch as D
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

          st <- H.get
          _  <- H.query (Slot 1)
                  $ H.action
                  $ Container
                  $ ContainerReceiver { render: renderContainer, items: difference st.items st.selected }

          H.liftAff $ log "List of selections..." *> logShow st.selected


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
