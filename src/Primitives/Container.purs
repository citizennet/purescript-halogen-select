module Select.Primitives.Container where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (seeks, store)
import Control.Monad.Aff.Console (log)
import Control.Monad.Aff.Class (class MonadAff)
import Data.Array (length, (!!))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import DOM.Event.Types as ET
import DOM.Event.Event (preventDefault)
import DOM.Event.KeyboardEvent as KE
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select.Primitives.State (updateStore, getState, State)
import Select.Effects (Effects)


-- | The query type for the `Container` primitive.
-- |
-- | - `Highlight`: Change the highlighted item to the next, previous, or a specific index.
-- | - `Select`: Select an item at the specified index
-- | - `Key`: Capture key events for arrow navigation, Escape to close, and Enter to select.
-- | - `Mouse`: Capture mouse events to close the menu or select an item
-- | - `Blur`: Trigger the DOM blur event
-- | - `Visibility`: Set the visibility by toggling, setting to on, or setting to off.
-- | - `ContainerReceiver`: Update the component on new `Input` when the parent re-renders.
data ContainerQuery o item a
  = Highlight Target a
  | Select Int a
  | Key KE.KeyboardEvent a
  | Mouse MouseState a
  | Blur a
  | Visibility VisibilityStatus a
  | ReplaceItems (Array item) a
  | Raise (o Unit) a
  | ContainerReceiver (ContainerInput o item) a

-- | For targeting items in the container via the `ContainerQuery`'s `Highlight` constructor
data Target
  = Next
  | Prev
  | Index Int

-- | For maintaining the state of the mouse in the `Container`
data MouseState
  = Down
  | Up

-- | For showing or hiding the `Container`
data VisibilityStatus
  = On
  | Off
  | Toggle

-- | The internal state of the `Container` primitive
-- |
-- | - `items`: An array of items held within the `Container`
-- | - `open`: Whether the `Container` is visible
-- | - `highlightedIndex`: The index of the highlighted item, if one exists
-- | - `lastIndex`: The index of the last item in the `Container`
-- | - `mouseDown`: Whether the mouse is clicked or not
type ContainerState item =
  { items            :: Array item
  , open             :: Boolean
  , highlightedIndex :: Maybe Int
  , lastIndex        :: Int
  , mouseDown        :: Boolean
  }

-- | The input type of the `Container` primitive
-- |
-- | - `items`: The initial value of `items` in the `ContainerState`
-- | - `render`: The `render` function for the `Container` primitive
type ContainerInput o item =
  { items  :: Array item
  , render :: ContainerState item -> H.ComponentHTML (ContainerQuery o item) }


-- | The Container sends the parent messages in two instances:
-- | - `Emit`: An embedded query has been triggered, and you must decide how to handle it; typically via evaluating
-- |           in the parent or re-routing the query to another primitive.
-- | - `ItemSelected`: An item has been selected from the container.
data Message o item
  = ItemSelected item
  | Emit (o Unit)

-- | The primitive handles state and transformations but defers all rendering to the parent. The
-- | render function can be written using our helper functions to ensure the right events are included. See the `Dispatch`
-- | module for more information.
component :: ∀ item o e m
  . MonadAff ( Effects e ) m
 => H.Component HH.HTML (ContainerQuery o item) (ContainerInput o item) (Message o item) m
component =
  H.component
    { initialState
    , render: extract
    , eval
    , receiver: HE.input ContainerReceiver
    }
  where
    initialState :: ContainerInput o item -> State (ContainerState item) (ContainerQuery o item)
    initialState i = store i.render
      { items: i.items
      , open: false
      , highlightedIndex: Nothing
      , lastIndex: length i.items - 1
      , mouseDown: false
      }

    eval
      :: (ContainerQuery o item)
      ~> H.ComponentDSL
          (State (ContainerState item) (ContainerQuery o item))
          (ContainerQuery o item)
          (Message o item)
          m
    eval = case _ of
      Raise q a -> do
        H.raise $ Emit q
        pure a

      Select index a -> do
        (Tuple _ st) <- getState
        if not st.open
          then pure a
          else a <$ case st.items !! index of
            Just item -> H.raise $ ItemSelected item
            _ -> H.liftAff $ log $ "Index " <> show index <> " is out of bounds."

      -- We can ignore the case in which we don't want anything highlighted
      -- as once the highlight becomes active, nothing but closing the menu
      -- will remove it
      Highlight target a -> do
        (Tuple _ st) <- getState

        if not st.open then pure a else a <$ case target of
          Index i -> H.modify
            $ seeks (_ { highlightedIndex = Just i } )

          Next    -> case st.highlightedIndex of
            Just i | i /= st.lastIndex -> H.modify
              $ seeks (_ { highlightedIndex = Just (i + 1) })
            otherwise -> H.modify
              $ seeks (_ { highlightedIndex = Just 0 })

          Prev    -> case st.highlightedIndex of
            Just i | i /= 0 -> H.modify
              $ seeks (_ { highlightedIndex = Just (i - 1) })
            otherwise -> H.modify
              $ seeks (_ { highlightedIndex = Just st.lastIndex })

      Key (ev :: KE.KeyboardEvent) a -> do
        (Tuple _ st) <- getState
        if not st.open then pure a else case KE.code ev of
          "Enter" -> do
            H.liftEff $ preventDefault $ KE.keyboardEventToEvent ev
            case st.highlightedIndex of
              Nothing -> pure a
              Just index -> eval $ Select index a

          "Escape" -> a <$ (H.modify $ seeks _ { open = false })

          "ArrowUp" -> a <$ do
            H.liftEff $ preventDefault $ KE.keyboardEventToEvent ev
            eval $ Highlight Prev a

          "ArrowDown" -> a <$ do
            H.liftEff $ preventDefault $ KE.keyboardEventToEvent ev
            eval $ Highlight Next a

          other -> pure a

      Mouse ms a -> do
        (Tuple _ st) <- getState
        if not st.open
          then pure a
          else a <$ case ms of
            Down -> H.modify $ seeks (_ { mouseDown = true })
            Up   -> H.modify $ seeks (_ { mouseDown = false })

      Blur a -> do
        (Tuple _ st) <- getState
        if not st.open || st.mouseDown
          then pure a
          else a <$ (eval $ Visibility Off a)

      -- When toggling, the user will lose their highlighted index.
      Visibility status a -> a <$ case status of
        On     -> H.modify $ seeks (_ { open = true })
        Off    -> H.modify $ seeks (_ { open = false, highlightedIndex = Nothing })
        Toggle -> H.modify $ seeks (\st -> st { open = not st.open
                                              , highlightedIndex = Nothing })

      ReplaceItems newItems a -> a <$ do
        H.modify $ seeks (_ { items = newItems })

      ContainerReceiver i a -> a <$ do
        H.modify
          $ updateStore i.render
          $ _ { items = i.items
              , highlightedIndex = Nothing
              , lastIndex = length i.items - 1 }


-- | Intended for use on a clickable toggle to show/hide the `Container` primitive. If
-- | you are using a text field to manage the container, use `getInputProps` instead.
getToggleProps :: ∀ o item e f.
   (ContainerQuery o item Unit -> Unit -> f Unit)
   -> Array
        (H.IProp
           ( onClick :: ET.MouseEvent
           , onKeyDown :: ET.KeyboardEvent
           , onMouseDown :: ET.MouseEvent
           , onMouseUp :: ET.MouseEvent
           , onBlur :: ET.FocusEvent
           , tabIndex :: Int
           | e
           )
           f
        )
  -> Array
       (H.IProp
          ( onClick :: ET.MouseEvent
          , onKeyDown :: ET.KeyboardEvent
          , onMouseDown :: ET.MouseEvent
          , onMouseUp :: ET.MouseEvent
          , onBlur :: ET.FocusEvent
          , tabIndex :: Int
          | e
          )
          f
       )
getToggleProps q = flip (<>)
  [ HE.onClick      $ HE.input_ $ q $ H.action $ Visibility Toggle
  , HE.onKeyDown    $ HE.input  $ \ev -> q $ H.action $ Key ev
  , HE.onMouseDown  $ HE.input_ $ q $ H.action $ Mouse Down
  , HE.onMouseUp    $ HE.input_ $ q $ H.action $ Mouse Up
  , HE.onBlur       $ HE.input_ $ q $ H.action $ Blur
  , HP.tabIndex 0
  ]

-- | Intended to allow you to embed whatever you want into the container
getChildProps :: ∀ o item e.
  Array
    (H.IProp
      ( onBlur :: ET.FocusEvent
      , tabIndex :: Int
      | e
      )
    (ContainerQuery o item)
    )
  ->
  Array
    (H.IProp
      ( onBlur :: ET.FocusEvent
      , tabIndex :: Int
      | e
      )
    (ContainerQuery o item)
    )
getChildProps = flip (<>)
  [ HE.onBlur      $ HE.input_ $ Blur
  , HP.tabIndex 0
  ]

-- | Intended to be used on the container primitive to capture key, click, highlighting, and other events
getContainerProps :: ∀ o item e.
   Array
     (H.IProp
        ( onMouseDown :: ET.MouseEvent
        , onMouseUp :: ET.MouseEvent
        , onBlur :: ET.FocusEvent
        , tabIndex :: Int
        | e
        )
        (ContainerQuery o item)
     )
   ->
   Array
      (H.IProp
         ( onMouseDown :: ET.MouseEvent
         , onMouseUp :: ET.MouseEvent
         , onBlur :: ET.FocusEvent
         , tabIndex :: Int
         | e
         )
         (ContainerQuery o item)
      )
getContainerProps = flip (<>)
  [ HE.onMouseDown $ HE.input_ $ Mouse Down
  , HE.onMouseUp   $ HE.input_ $ Mouse Up
  , HE.onBlur      $ HE.input_ $ Blur
  , HP.tabIndex 0
  ]

-- | Intended to be used on items inside the container
getItemProps :: ∀ o item e
  . Int
  -> Array
      (H.IProp
         ( onClick :: ET.MouseEvent
         , onMouseOver :: ET.MouseEvent
         , onKeyDown :: ET.KeyboardEvent
         , onBlur :: ET.FocusEvent
         , tabIndex :: Int
         | e
         )
         (ContainerQuery o item)
      )
  -> Array
      (H.IProp
        ( onClick :: ET.MouseEvent
        , onMouseOver :: ET.MouseEvent
        , onKeyDown :: ET.KeyboardEvent
        , onBlur :: ET.FocusEvent
        , tabIndex :: Int
        | e
        )
        (ContainerQuery o item)
      )
getItemProps index = flip (<>)
  [ HE.onClick     $ HE.input_ $ Select index
  , HE.onMouseOver $ HE.input_ $ Highlight (Index index)
  , HE.onKeyDown   $ HE.input  $ \ev -> Key ev
  , HE.onBlur      $ HE.input_ $ Blur
  , HP.tabIndex 0
  ]
