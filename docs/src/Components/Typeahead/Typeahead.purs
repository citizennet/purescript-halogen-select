module Example.Component.Typeahead where

import Prelude

import Control.Monad.Eff.Timer (setTimeout, TIMER)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (log, logShow, CONSOLE)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import DOM (DOM)
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

type TypeaheadItem = String
type Effects eff = ( timer :: TIMER, avar :: AVAR, dom :: DOM, console :: CONSOLE | eff )

data Query a
  = Log String a
  | HandleContainer (C.Message Query TypeaheadItem) a
  | HandleSearch    (S.Message Query TypeaheadItem) a
  | Removed TypeaheadItem a

type ChildSlot = Either2 Unit Unit
type ChildQuery e
  = Coproduct2 (C.ContainerQuery Query TypeaheadItem)
               (S.SearchQuery Query TypeaheadItem (Effects e))

type State =
  { items    :: Array TypeaheadItem
  , selected :: Array TypeaheadItem }

type Input = Array String

data Message = Void

component :: ∀ m e
  . MonadAff ( Effects e ) m
 => H.Component HH.HTML Query Input Message m
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: Input -> State
    initialState i = { items: i, selected: [] }

    render :: State -> H.ParentHTML Query (ChildQuery e) ChildSlot m
    render st =
      HH.div_
        [ renderSelections st.selected
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
            { render: renderContainer, items: st.items }
            ( HE.input HandleContainer )
        ]

    eval :: Query ~> H.ParentDSL State Query (ChildQuery e) ChildSlot Message m
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

          H.liftAff $ log $ "New search performed: " <> s

          let filtered  = filterItems s st.items
          let available = difference filtered st.selected

          -- Allow insertion of elements
          let newItems
                | length available < 1 = s : available
                | otherwise            = available

          _ <- H.query' CP.cp1 unit
                 $ H.action
                 $ C.ContainerReceiver { render: renderContainer, items: newItems }

          -- Test custom effects
          _ <- H.liftEff $ setTimeout 100 (pure unit)
          pure a


      HandleContainer m a -> case m of
        C.Emit q -> eval q *> pure a

        C.ItemSelected item -> a <$ do
          st <- H.get
          if length (filter ((==) item) st.items) > 0
            then H.modify _ { selected = ( item : st.selected ) }
            else H.modify _
                  { items = ( item : st.items )
                  , selected = ( item : st.selected ) }

          newSt <- H.get
          _  <- H.query' CP.cp1 unit
                  $ H.action
                  $ C.ContainerReceiver
                  $ { render: renderContainer
                    , items: difference newSt.items newSt.selected }

          H.liftAff $ log "List of selections..." *> logShow newSt.selected

      Removed item a -> a <$ do
        st <- H.get
        H.modify _ { selected = filter ((/=) item) st.selected }

        newSt <- H.get
        _  <- H.query' CP.cp1 unit
                $ H.action
                $ C.ContainerReceiver
                $ { render: renderContainer
                  , items: difference newSt.items newSt.selected }

        H.liftAff $ log "List of selections..." *> logShow newSt.selected


{-

HELPERS

-}

filterItems :: TypeaheadItem -> Array TypeaheadItem -> Array TypeaheadItem
filterItems str = filter (\i -> contains (Pattern str) i)

{-

Config

-}

class_ = HP.class_ <<< HH.ClassName

renderSearch :: ∀ e
  . (S.SearchState e)
 -> H.HTML Void (S.SearchQuery Query TypeaheadItem e)
renderSearch _ = HH.input
  ( S.getInputProps
    [ class_ "rounded-sm bg-white w-full flex py-2 px-3"
    , HP.placeholder "Type to search..." ]
  )

-- Render function to pass to the child container component
renderContainer
  :: (C.ContainerState TypeaheadItem)
  -> H.HTML Void (C.ContainerQuery Query TypeaheadItem)
renderContainer st =
  HH.div [ class_ "relative z-50" ]
  $ if not st.open
    then [ ]
    else [ renderItems $ renderItem `mapWithIndex` st.items ]
  where
    -- Render the container for the items
    renderItems
      :: Array (H.HTML Void (C.ContainerQuery Query TypeaheadItem))
      -> H.HTML Void (C.ContainerQuery Query TypeaheadItem)
    renderItems html =
      HH.div
      ( C.getContainerProps
        [ class_ "absolute bg-white shadow rounded-sm pin-t pin-l w-full" ]
      )
      [ HH.ul [ class_ "list-reset" ] html ]

    renderItem
      :: Int
      -> TypeaheadItem
      -> H.HTML Void (C.ContainerQuery Query TypeaheadItem)
    renderItem index item =
      HH.li ( C.getItemProps index props ) [ HH.text item ]
      where
        props = [ class_
          $ "px-4 py-1 text-grey-darkest"
          <> if st.highlightedIndex == Just index
               then " bg-grey-lighter"
               else "" ]


renderSelections items =
  if length items == 0
    then HH.div_ []
    else
    HH.div
    [ class_ "bg-white rounded-sm w-full border-b border-grey-lighter" ]
    [ HH.ul
      [ class_ "list-reset" ]
      ( renderSelectedItem <$> items )
    ]
  where
    renderSelectedItem item =
      HH.li
      [ class_ "px-4 py-1 text-grey-darkest hover:bg-grey-lighter relative" ]
      [ HH.span_ [ HH.text item ]
      , closeButton item
      ]

    closeButton item =
      HH.span
      [ HE.onClick $ HE.input_ (Removed item)
      , class_ "absolute pin-t pin-b pin-r p-1 mx-3 cursor-pointer" ]
        [ HH.text "×" ]
