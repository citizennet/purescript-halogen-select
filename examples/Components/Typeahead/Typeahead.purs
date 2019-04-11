module Docs.Components.Typeahead where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AR
import Data.Argonaut.Decode ((.:), decodeJson)
import Data.Array (mapWithIndex, filter, (:), (!!))
import Data.Foldable (for_, length)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Docs.CSS as CSS
import Docs.Internal.RemoteData as RD
import Docs.Components.Dropdown as Dropdown
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select as Select
import Select.Setters as Setters

-----
-- Spec

-- Just like the dropdown, all we have to do to export a full component is just 
-- specialize the types and supply a render function, query handler, and message 
-- handler.
spec :: forall m. MonadAff m => Select.Spec ExtraState ExtraQuery ChildSlots ExtraMessage m
spec = Select.defaultSpec 
  { render = render
  , handleQuery = handleQuery
  , handleMessage = handleMessage
  }

-----
-- State

-- Our typeahead will fetch its data remotely, which means that we won't maintain
-- a list of items in state. We'll represent our items as remote data which may be 
-- present but may also be loading or in an error state.
type ExtraState =
  ( selections :: Array Location 
  , available :: RD.RemoteData String (Array Location)
  )

-- Our state and input types can now be extended with these
type State = Select.State ExtraState
type Input = Select.Input ExtraState

-----
-- Child Components

-- We're going to embed a dropdown in the component, so we'll extend Select with
-- new child slots.
type ChildSlots = 
  ( dropdown :: H.Slot Dropdown.Query Dropdown.Message Unit )

_dropdown = SProxy :: SProxy "dropdown"

-----
-- Messages

-- We'll add a new message to Select to cover the case when an item is removed. The
-- parent might wish to display a confirmation of the last-removed item or use it
-- otherwise.
data ExtraMessage 
  = ItemRemoved Location 

type Message = Select.Message ExtraMessage

-- We also need to take action when an item is selected in Select or a debounced
-- search has completed. We'll do that by handling the message within Select.
handleMessage 
  :: forall m
   . MonadAff m
  => Message 
  -> H.HalogenM State Action ChildSlots Message m Unit
handleMessage = case _ of
  Select.Selected ix -> do
    st <- H.get
    case st.available of 
      RD.Success arr -> do
        let newSelections = fromMaybe st.selections $ (_ : st.selections) <$> (arr !! ix)
        H.modify_ _ { selections = newSelections, search = "" } 
      _ -> pure unit
  
  Select.Searched str -> do
    st <- H.get

    -- we'll use an external api to search locations 
    H.modify_ _ { available = RD.Loading }
    items <- searchLocations str
    H.modify_ _ { available = items, lastIndex = maybe 0 (\arr -> length arr - 1) $ RD.toMaybe items }

    -- and then highlight the first one
    handleAction $ Select.Highlight $ Select.Index 0
  
  _ -> 
    pure unit
  where
  handleAction act = Select.handleAction handleQuery handleMessage act


-----
-- Query

-- We introduced some new behavior for selecting items via the `handleMessage` function
-- but we also need to support removal and we need to handle messages output by our
-- further child component, the dropdown.
type Query = Select.Query ExtraQuery ChildSlots

-- We'll provide a synonym for the action type, too.
type Action = Select.Action ExtraState ExtraQuery ChildSlots

data ExtraQuery a
  = Remove Location a
  | HandleDropdown Dropdown.Message a

handleQuery 
  :: forall m a
   . MonadAff m
  => ExtraQuery a
  -> H.HalogenM State Action ChildSlots Message m (Maybe a)
handleQuery = case _ of  
  Remove item a -> Just a <$ do
    st <- H.get
    let newSelections = filter (_ /= item) st.selections
    H.modify_ _ { selections = newSelections }
    H.raise $ Select.Raised $ ItemRemoved item
  
  -- We can also handle our child component
  HandleDropdown msg a -> Just a <$ case msg of
    Select.Raised (Dropdown.SelectionChanged (Tuple oldSelection newSelection)) -> do
      st <- H.get
      let 
	mkLocation str = { name: "User Added: " <> str, population: "1" }
        newSelections = case oldSelection, newSelection of
          Nothing, Nothing -> Nothing
	  Nothing, Just str -> Just (mkLocation str : st.selections)
          Just str, Nothing -> Just (filter (_ /= mkLocation str) st.selections)
          Just old, Just new -> Just (mkLocation new : (filter (_ /= mkLocation old) st.selections))
      for_ newSelections \selections -> 
        H.modify_ _ { selections = selections }
    _ -> 
      pure unit

-----
-- Render Function

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render state = 
  HH.div_ 
    [ renderSelections, renderInput, renderContainer ]
  where
  renderSelections :: forall props. HH.HTML props Action
  renderSelections = case length state.selections of
    0 -> 
      HH.div_ []
    _ ->
      HH.div
        [ class_ "bg-white rounded-sm w-full border-b border-grey-lighter" ]
        [ HH.ul
          [ class_ "list-reset" ]
          (renderSelectedItem <$> state.selections)
        ]
    where
    renderSelectedItem item =
      HH.li
        [ class_ "px-4 py-1 text-grey-darkest hover:bg-grey-lighter relative" ]
        [ renderLocation item
        , closeButton item
        ]

    closeButton item =
      HH.span
        [ HE.onClick \_ -> Just $ Select.AsAction $ Select.Embed $ H.tell $ Remove item
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
      ([ renderItems (mapWithIndex renderItem) ] # guard (state.visibility == Select.On))
    where
    -- here we can render a further child component, the dropdown, which is *also*
    -- a select component.
    renderChild = HH.slot _dropdown unit (Select.component Dropdown.spec) input handleChild
      where
      handleChild msg = Just (Select.AsAction (Select.Embed (H.tell (HandleDropdown msg))))
      input = 
        { inputType: Select.Toggle
	, search: Nothing
	, debounceTime: Nothing
        , lastIndex: 1

        -- extensions
	, items: [ "Earth", "Mars" ]
	, selection: Nothing
	}

    renderItems f = do
      let renderMsg msg = [ HH.span [ class_ "pa-4" ] [ HH.text msg ] ]
      HH.div
        ( Setters.setContainerProps
          [ class_ "absolute bg-white shadow rounded-sm pin-t pin-l w-full" ]
        )
	case state.available of
            RD.NotAsked -> renderMsg "No search performed..."
	    RD.Loading -> renderMsg "Loading..."
            RD.Failure e -> renderMsg e
	    RD.Success available -> 
	     [ HH.ul 
                 [ class_ "list-reset" ] 
                 (f available) 
             , renderChild
	     ]

    renderItem index item =
      HH.li (Setters.setItemProps index [ class_ (base <> extra) ]) [ renderLocation item ]
      where
      base = "px-4 py-1 text-grey-darkest"
      extra = " bg-grey-lighter" # guard (state.highlightedIndex == Just index) 


-----
-- Helpers

class_ :: ∀ p i. String -> HH.IProp ( "class" :: String | i ) p
class_ = HP.class_ <<< HH.ClassName

-----
-- Async

type Location =
  { name :: String
  , population :: String
  }

renderLocation :: forall t0 t1. Location -> HH.HTML t0 t1
renderLocation { name, population } = 
  HH.div_
    [ HH.text name
    , HH.span
        [ class_ "ml-4 text-grey-lighter" ]
	[ HH.text population ]
    ]

searchLocations 
  :: forall m
   . MonadAff m
  => String
  -> m (RD.RemoteData String (Array Location))
searchLocations search = liftAff do
  res <- AX.get AR.json ("https://swapi.co/api/planets/?search=" <> search) 
  let body = lmap AR.printResponseFormatError res.body
  pure $ RD.fromEither $ traverse decodeJson =<< (_ .: "results") =<< decodeJson =<< body

