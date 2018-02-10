module Example.Component.Dropdown where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log, logShow, CONSOLE)
import CSS as CSS
import DOM (DOM)
import DOM.Event.KeyboardEvent as KE
import Data.Array ((:), difference, mapWithIndex, length, delete)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS as HC
import Select.Primitives.Container as C

type DropdownItem = String

type State =
  { items    :: Array DropdownItem
  , selected :: Array DropdownItem }

data Query a
  = Log String a
  | HandleContainer (C.Message Query DropdownItem) a
  | ToContainer (C.ContainerQuery Query DropdownItem Unit) a
  | Removed DropdownItem a

type Effects eff = ( dom :: DOM, console :: CONSOLE | eff )

component :: ∀ e
  . H.Component HH.HTML Query Unit Void (Aff (Effects e))
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

    render
      :: State
      -> H.ParentHTML
           Query
           (C.ContainerQuery Query DropdownItem)
           Unit
           (Aff (Effects e))
    render st =
      HH.div_
      [ renderSelections st.selected
      , renderToggle
      , HH.slot
          unit
          C.component
          { items: testData, render: renderContainer }
          (HE.input HandleContainer)
      ]

    -- Here, Menu.Emit recursively calls the parent eval function.
    -- Menu.Selected item is handled by removing that item from
    -- the options and maintaining it here in state.
    eval
      :: Query
      ~> H.ParentDSL
           State
           Query
           (C.ContainerQuery Query DropdownItem)
           Unit
           Void
           (Aff (Effects e))
    eval = case _ of
      Log s a -> H.liftAff (log s) *> pure a

      ToContainer q a -> H.query unit q *> pure a

      HandleContainer m a -> case m of
        C.Emit q -> eval q *> pure a

        C.ItemSelected item -> do
          H.liftAff $ log ("Selected: " <> item)
          H.modify \st -> st { selected = ( item : st.selected ) }

          st <- H.get
          H.liftAff $ logShow st.selected
          _  <- H.query unit
                  $ H.action
                  $ C.ContainerReceiver
                  $ { render: renderContainer
                    , items: difference st.items st.selected }
          pure a

      Removed item a -> do
        st <- H.get
        let newSelections = delete item st.selected
            newItems = difference newSelections st.items

        H.modify (_ { selected = newSelections })

        _  <- H.query unit
                $ H.action
                $ C.ContainerReceiver
                $ { render: renderContainer
                  , items: newItems }

        pure a



{-

HELPERS

-}

class_ = HP.class_ <<< HH.ClassName

-- The parent must provide some input data.
testData :: Array DropdownItem
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
  , "THE COOKIE MONSTER DID NOTHING WRONG" ]


-- Render whatever is going to provide the action for
-- toggling the menu. Notably, this is not a primitive
renderToggle :: ∀ e
  . H.ParentHTML
      Query
      (C.ContainerQuery Query DropdownItem)
      Unit
      (Aff (Effects e))
renderToggle =
  HH.div
    ( C.getToggleProps ToContainer
      [ HE.onMouseOver $ HE.input_ $ Log "I'm the parent."
      , class_ "rounded-sm bg-white w-full flex py-1 px-1"
      ]
    )
    [ HH.p
      [ class_ "text-grey-dark py-1 px-3 flex-auto" ]
      [ HH.text "Select an option" ]
    , HH.p
      [ class_ "font-medium text-blue-light border-l ml-1 px-3 text-sm cursor-pointer self-center" ]
      [ HH.text "Browse" ]
    ]

-- Render function to pass to the child container component
renderContainer
  :: (C.ContainerState DropdownItem)
  -> H.HTML Void (C.ContainerQuery Query DropdownItem)
renderContainer st =
  HH.div [ class_ "relative" ]
  $ if not st.open
    then [ ]
    else [ renderItems $ renderItem `mapWithIndex` st.items ]
  where
    -- Render the container for the items
    renderItems
      :: Array (H.HTML Void (C.ContainerQuery Query DropdownItem))
      -> H.HTML Void (C.ContainerQuery Query DropdownItem)
    renderItems html =
      HH.div
      ( C.getContainerProps
        [ class_ "absolute bg-white shadow rounded-sm pin-t pin-l w-full" ]
      )
      [ HH.ul [ class_ "list-reset" ] html ]

    renderItem
      :: Int
      -> DropdownItem
      -> H.HTML Void (C.ContainerQuery Query DropdownItem)
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
      [ class_ "px-4 py-1 hover:bg-grey-lighter relative" ]
      [ HH.span_ [ HH.text item ]
      , closeButton item
      ]

    closeButton item =
      HH.span
      [ HE.onClick $ HE.input_ (Removed item)
      , class_ "absolute pin-t pin-b pin-r p-1 mx-3 cursor-pointer" ]
        [ HH.text "×" ]
