module Docs.Components.Typeahead where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Aff.AVar (AVAR)
import DOM (DOM)
import Data.Array (mapWithIndex, difference, filter, (:))
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.Time.Duration (Milliseconds(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Select.Primitives.SearchContainer as SC
import Select.Primitives.Container as C
import Select.Primitives.Search as S

type TypeaheadItem = String

type Effects eff = ( avar :: AVAR, dom :: DOM, console :: CONSOLE | eff )

data Query a
  = Log String a
  | HandleSearchContainer (SC.Message Query TypeaheadItem) a
  | Removed TypeaheadItem a

type State =
  { items    :: Array TypeaheadItem
  , selected :: Array TypeaheadItem }

type Input = Array String
data Message = Void

type ChildSlot = Unit
type ChildQuery eff = SC.SearchContainerQuery Query TypeaheadItem eff

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

    render :: State -> H.ParentHTML Query (ChildQuery (Effects e)) ChildSlot m
    render st =
      HH.div
        [ class_ "w-full" ]
        [ renderSelections st.selected
        , HH.slot unit SC.component searchContainerInput (HE.input HandleSearchContainer)
        ]
      where
        searchContainerInput =
          { search: Nothing
          , debounceTime: Milliseconds 0.0
          , items: difference st.items st.selected
          , renderSearch
          , renderContainer
          }

    eval :: Query ~> H.ParentDSL State Query (ChildQuery (Effects e)) ChildSlot Message m
    eval = case _ of
      Log str a -> a <$ do
        H.liftAff $ log str

      HandleSearchContainer m a -> case m of
        SC.Emit q -> eval q *> pure a

        SC.SearchMessage m' -> case m' of
          S.NewSearch s -> do
            st <- H.get
            let filtered  = filterItems s st.items
                available = difference filtered st.selected
            _ <- H.query unit <<< SC.inContainer $ C.ReplaceItems available
            pure a

          otherwise -> pure a

        SC.ContainerMessage m' -> case m' of
          C.ItemSelected item -> do
            st <- H.get
            if length (filter ((==) item) st.items) > 0
              then H.modify _ { selected = ( item : st.selected ) }
              else H.modify _
                    { items = ( item : st.items )
                    , selected = ( item : st.selected ) }

            newSt <- H.get
            let newItems = difference newSt.items newSt.selected
            _  <- H.query unit <<< SC.inContainer $ C.ReplaceItems newItems
            pure a

          otherwise -> pure a


      Removed item a -> do
        st <- H.get
        H.modify _ { selected = filter ((/=) item) st.selected }

        newSt <- H.get
        let newItems = difference newSt.items newSt.selected
        _  <- H.query unit <<< SC.inContainer $ C.ReplaceItems newItems
        pure a


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
