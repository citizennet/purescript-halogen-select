module Docs.Components.Typeahead where

import Prelude

import Data.Array (elemIndex, mapWithIndex, difference, filter, (:))
import Data.Const (Const)
import Data.Foldable (for_, length)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.String (Pattern(..), contains)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj, match)
import Docs.CSS as CSS
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select as Select
import Select.Setters as Setters

-----
-- Parent

type TypeaheadItem = String

data Action
  = HandleSelect SelectMessage

type ChildSlots = 
  ( select :: H.Slot SelectQuery SelectMessage Unit 
  )

_select = SProxy :: SProxy "select"

component :: ∀ m. MonadAff m => H.Component HH.HTML (Const Void) Unit Void m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction :: Action -> H.HalogenM Unit Action ChildSlots Void m Unit
  handleAction = case _ of
    HandleSelect msg -> match
      { searched: \search -> do
          _ <- H.query _select unit $ H.tell $ Select.PerformAction $ handleSearch search
	  pure unit
      
      , selected: \item -> do
          _ <- H.query _select unit $ H.tell $ Select.PerformAction $ handleSelected item
	  pure unit
      
      , visibilityChanged: \_ -> pure unit

      -- extensible!
      , itemRemoved: \_ -> pure unit
      } msg

  render :: Unit -> H.ComponentHTML Action ChildSlots m
  render _ =
    HH.div
      [ class_ "w-full" ]
      [ HH.slot _select unit (Select.component renderSelect handleExtraActions) input (Just <<< HandleSelect) ]
    where
    input =
      { search: Nothing
      , debounceTime: Nothing
      , inputType: Select.TextInput
      , items: [ "d'angelo", "susy", "george", "hao", "patricia", "ferdinand" ]

      -- extensions
      , selectedItems: [] 
      , keepOpen: true
      , embeddedMessage: "Hey, I'm an embedded message"
      }


----------
-- Select + Extensions

{- 
  Select can be extended in several ways:
    - you can add new fields in state which can be accessed in rendering and in action handlers
    - you can add new messages as outputs of the component
    - you can add new actions to add to the behaviors of the component
    - you can add new child components and interact with them through Select
    - and, of course, you can do whatever you'd like with the render function
-}

type SelectQuery = Select.Query TypeaheadItem ExtraActions ()

-----
-- State

type SelectState = Select.State TypeaheadItem ExtraState

type ExtraState =
  ( selectedItems :: Array TypeaheadItem
  , keepOpen :: Boolean
  , embeddedMessage :: String
  )

-----
-- Messages

type SelectMessage = Select.Message TypeaheadItem ExtraMessages

type ExtraMessages = 
  ( itemRemoved :: TypeaheadItem 
  )

-----
-- Actions

type SelectAction = Select.Action ExtraActions

type ExtraActions = 
  ( remove :: TypeaheadItem
  , logInfo :: String
  , handleSearch :: String
  , handleSelected :: String
  )

remove :: String -> SelectAction
remove = inj (SProxy :: _ "remove")

logInfo :: String -> SelectAction
logInfo = inj (SProxy :: _ "logInfo")

handleSearch :: String -> SelectAction
handleSearch = inj (SProxy :: _ "handleSearch")

handleSelected :: String -> SelectAction
handleSelected = inj (SProxy :: _ "handleSelected")

handleExtraActions 
  :: forall m
   . MonadAff m
  => Variant ExtraActions 
  -> H.HalogenM SelectState SelectAction () SelectMessage m Unit
handleExtraActions = match  
  { logInfo: \str -> 
      Console.log str 

  , remove: \item ->
      H.modify_ \st -> st { selectedItems = filter (_ /= item) st.selectedItems }
  
  , handleSearch: \search -> do
      st <- H.get
      let 
        newItems = difference (filterItems search st.items) st.selectedItems
        index = elemIndex search st.items
      for_ index \ix -> do
        Select.handleAction handleExtraActions $ Select.highlight $ Select.Index ix
  
  , handleSelected: \item -> do
      st <- H.get
      when (not st.keepOpen) do
        Select.handleAction handleExtraActions $ Select.setVisibility Select.Off
      H.modify_ _ { selectedItems = (item : st.selectedItems) }
  }

-----
-- Rendering

renderSelect :: forall m. SelectState -> H.ComponentHTML SelectAction () m
renderSelect state = 
  HH.div_ 
    [ renderSelections, renderInput, renderContainer ]
  where
  renderSelections :: forall props. HH.HTML props SelectAction
  renderSelections = case length state.selectedItems of
    0 -> 
      HH.div_ []
    _ ->
      HH.div
        [ class_ "bg-white rounded-sm w-full border-b border-grey-lighter" ]
        [ HH.ul
          [ class_ "list-reset" ]
          (renderSelectedItem <$> state.selectedItems)
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
        [ HE.onClick \_ -> Just (remove item)
        , class_ "absolute pin-t pin-b pin-r p-1 mx-3 cursor-pointer" 
        ]
        [ HH.text "×" ]
  
  renderInput = HH.input $ Setters.setInputProps
    [ HP.classes CSS.input
    , HP.placeholder "Type to search..." 
    ]

  renderContainer =
    HH.div 
      [ class_ "relative z-50" ]
      if state.visibility == Select.Off
        then []
        else [ renderItems $ renderItem `mapWithIndex` state.items ]
    where
    renderChild =
      HH.div
        [ HE.onClick \_ -> Just $ logInfo "The embedded message was clicked" ]
        [ HH.text state.embeddedMessage ]

    renderItems html =
      HH.div
        ( Setters.setContainerProps
          [ class_ "absolute bg-white shadow rounded-sm pin-t pin-l w-full" ]
        )
        [ renderChild
        , HH.ul 
            [ class_ "list-reset" ] 
            html 
        ]

    renderItem index item =
      HH.li (Setters.setItemProps index [ class_ classString ]) [ HH.text item ]
      where
      classString = 
        "px-4 py-1 text-grey-darkest" <> " bg-grey-lighter" # guard (state.highlightedIndex == Just index) 


----------
-- Helpers

class_ :: ∀ p i. String -> HH.IProp ( "class" :: String | i ) p
class_ = HP.class_ <<< HH.ClassName

filterItems :: TypeaheadItem -> Array TypeaheadItem -> Array TypeaheadItem
filterItems str = filter (\i -> contains (Pattern str) i)

