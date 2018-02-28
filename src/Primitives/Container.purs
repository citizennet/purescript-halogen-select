-- | The Container primitive contains an array of items that can be selected. As
-- | you provide the render functions, this array of items could be anything from
-- | a dropdown list to a calendar grid. The primitive provides keyboard and click
-- | behaviors for highlighting and selecting items and will be used in nearly every
-- | selection component you write.

module Select.Primitives.Container where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (seeks, store)
import Control.Monad.Aff.Class (class MonadAff)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.Event.KeyboardEvent as KE
import DOM.Event.Types as ET
import Data.Array (length, (!!))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select.Primitives.State (updateStore, getState, State)


-- | The query type for the `Container` primitive. This primitive handles selections
-- | and keyboard navigation.
-- |
-- | Arguments:
-- |
-- | - `o`: The type of the parent query to embed. This is usually the component that
-- | -      mounts the search primitive, but it could also be the query type of some
-- |        higher component.
-- | - `item`: Your custom item type, used by your renderers.
-- |
-- | Constructors:
-- |
-- | - `Highlight`: Change the highlighted item to the next, previous, or a specific index.
-- | - `Select`: Select an item at the specified index
-- | - `Key`: Capture key events for arrow navigation, Escape to close, and Enter to select.
-- | - `Mouse`: Capture mouse events to close the menu or select an item
-- | - `Blur`: Trigger the DOM blur event
-- | - `SetVisibility`: Set the visibility to on or off.
-- | - `ToggleVisibility`: Toggle the visibility from its current status.
-- | - `ReplaceItems`: Replace the array of items.
-- | - `Raise`: Embed a parent query that can be returned to the parent for evaluation.
-- | - `ContainerReceiver`: Update the component on new `Input` when the parent re-renders.
data ContainerQuery o item a
  = Highlight Target a
  | Select Int a
  | Key KE.KeyboardEvent a
  | Mouse MouseState a
  | Blur a
  | SetVisibility VisibilityStatus a
  | ToggleVisibility a
  | ReplaceItems (Array item) a
  | Raise (o Unit) a
  | ContainerReceiver (ContainerInput o item) a

-- | Navigation for the container's `Highlight` query.
-- |
-- | Note: in `eval`, this wraps around, so providing `Highlight Next` on the
-- | last item in the array will highlight the first item in the array.
data Target
  = Next
  | Prev
  | Index Int

-- | Maintain the state of the mouse in the container's `Mouse` query.
data MouseState
  = Down
  | Up

-- | Possible visibility statuses for the container's `Visibility` query.
-- |
-- | `On`: Set the container to be visible (use this to "open" the container)
-- | `Off`: Set the container to be visible (use this to "close" the container)
-- | `Toggle`: Toggle between "open" and "closed".
data VisibilityStatus
  = On
  | Off

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


-- | Messages emitted by the container primitive to notify the parent of important events.
-- |
-- | - `ItemSelected`: An item has been selected in the container. This does not indicate the item
-- |                   has been removed; if you would like the item to also be removed from the
-- |                   container, make sure to query `ReplaceItems` from the parent.
-- | - `ContainerClicked`: The container has been clicked, which draws focus away from the
-- |                       search if used with a search primitive, this message lets the search
-- |                       container know to return focus to it
-- | - `VisibilitySet`: Notifies the parent that the container visibility has a new value.
-- |                    Useful when you need to trigger some behavior (like validation) when the user closes or opens the menu.
-- | - `Emit`: A parent query has been triggered and should be evaluated by the parent. Typically:
-- |
-- | ```purescript
-- | eval (HandleContainer (Emit q) next) = eval q *> pure next
-- | ```
data Message o item
  = ItemSelected item
  | ContainerClicked
  | VisibilitySet VisibilityStatus
  | Emit (o Unit)

-- | The primitive handles state and transformations but defers all rendering to the parent. The
-- | render function can be written using our helper functions to ensure the right events are included.
component :: ∀ o item eff m
  . MonadAff ( dom :: DOM | eff ) m
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
          else case st.items !! index of
            Just item -> (H.raise $ ItemSelected item) *> pure a
            _ -> pure a  -- This should be impossible

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
            Up   -> do
              H.modify $ seeks (_ { mouseDown = false })
              H.raise ContainerClicked

      Blur a -> do
        (Tuple _ st) <- getState
        if not st.open || st.mouseDown
          then pure a
          else a <$ (eval $ SetVisibility Off a)

      -- When toggling, the user will lose their highlighted index.
      SetVisibility status a -> a <$ do
        case status of
          On  -> H.modify $ seeks (_ { open = true })
          Off -> H.modify $ seeks (_ { open = false, highlightedIndex = Nothing })
        H.raise $ VisibilitySet status

      ToggleVisibility a -> a <$ do
        (Tuple _ st) <- getState
        case st.open of
          false -> eval $ SetVisibility On unit
          _     -> eval $ SetVisibility Off unit

      ReplaceItems newItems a -> a <$ do
        H.modify $ seeks (_ { items = newItems })

      ContainerReceiver i a -> a <$ do
        H.modify
          $ updateStore i.render
          $ _ { items = i.items
              , highlightedIndex = Nothing
              , lastIndex = length i.items - 1 }


-- | Attach properties to a DOM node that will maintain focus and capture key and click events
-- | for the container. If you are using the search primitive, this helper is unnecessary.
-- |
-- | Note: This function will overwrite any properties of the same name that are already set. Use
-- | it directly on the list of properties for the node that will serve as the toggle.
-- |
-- | Note: The toggle is not rendered within the container component. Instead, you should define
-- | a query on the parent that routes events to the relevant container, and provide that query
-- | as the first argument to this function.
-- |
-- | ```purescript
-- | -- ToContainer is a parent query you have defined that, in eval, sends queries to the container.
-- | span (getToggleProps ToContainer [ class_ $ ClassName "my-class" ]) [ text "Button" ]
-- | ```
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
  [ HE.onClick      $ HE.input_ $ q $ H.action $ ToggleVisibility
  , HE.onKeyDown    $ HE.input  $ \ev -> q $ H.action $ Key ev
  , HE.onMouseDown  $ HE.input_ $ q $ H.action $ Mouse Down
  , HE.onMouseUp    $ HE.input_ $ q $ H.action $ Mouse Up
  , HE.onBlur       $ HE.input_ $ q $ H.action $ Blur
  , HP.tabIndex 0
  ]

-- | A helper to embed your own HTML and queries inside the container's render function. It
-- | will ensure that your events do not inadvertently steal focus or trigger a blur on the
-- | container. Useful to embed buttons and other arbitrary HTML within the container.
-- |
-- | To embed your own queries, remember to use the `Raise` query from the container to wrap
-- | them. This will ensure they are re-raised to the parent.
-- |
-- | Use directly on the properties for the element you are embedding:
-- |
-- | ```purescript
-- | button (getChildProps [ onClick $ input_ $ Raise $ action $ MyQuery "Do something" ]) [ text "Button" ]
-- | ```
-- |
-- | Note: This will overwrite any properties of the same name.
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

-- | Attach properties to the HTML element that encloses the container. This makes sure you can
-- | select items without blurring the container, unless you want to. It is used in conjunction
-- | with the getItemProps helper, which you should attach to each item in the container.
-- |
-- | ```purescript
-- | div (getContainerProps [ class_ (ClassName "my-class") ]) [ ... further html ... ]
-- | ```
-- |
-- | Note: This function will overwrite any properties of the same name that are already set.
getContainerProps :: ∀ o item e.
   Array
     (H.IProp
        ( onMouseDown :: ET.MouseEvent
        , onMouseUp :: ET.MouseEvent
        | e
        )
        (ContainerQuery o item)
     )
   ->
   Array
      (H.IProp
        ( onMouseDown :: ET.MouseEvent
        , onMouseUp :: ET.MouseEvent
        | e
        )
        (ContainerQuery o item)
     )
getContainerProps = flip (<>)
  [ HE.onMouseDown $ HE.input_ $ Mouse Down
  , HE.onMouseUp   $ HE.input_ $ Mouse Up
  ]

-- | Attach events to an item in the container to support selection, highlighting, and key
-- | events (like Enter to select). Used in conjunction with `getContainerProps`.
-- |
-- | This function requires an index. It's usually easiest to provide a renderer that uses
-- | `mapWithIndex` to ensure indexes are provided properly.
-- |
-- | ```purescript
-- | renderItems arrayOfItems = renderItem `mapWithIndex` arrayOfItems
-- | renderItem index item = li (getItemProps index [ class_ (ClassName "item-class") ]) [ text item ]
-- | ```
-- |
-- | You should provide CSS to highlight items without requiring hovers, so that arrow keys
-- | can properly maintain highlights. To do that, use the container's state to check if the
-- | item being rendered has the same index as the highlight, and if so, apply your class:
-- |
-- | ```purescript
-- | -- in renderItem...
-- | if state.highlightedIndex == Just index then "highlight-class" else "no-highlight-class"
-- | ```
-- |
-- | Note: This function will overwrite any properties of the same name that are already set.
getItemProps :: ∀ o item e
  . Int
  -> Array
      (H.IProp
        ( onMouseDown :: ET.MouseEvent
        , onMouseOver :: ET.MouseEvent
        , onKeyDown :: ET.KeyboardEvent
        , onBlur :: ET.FocusEvent
        | e
        )
        (ContainerQuery o item)
      )
  -> Array
      (H.IProp
        ( onMouseDown :: ET.MouseEvent
        , onMouseOver :: ET.MouseEvent
        , onKeyDown :: ET.KeyboardEvent
        , onBlur :: ET.FocusEvent
        | e
        )
        (ContainerQuery o item)
      )
getItemProps index = flip (<>)
  [ HE.onMouseDown $ HE.input_ $ Select index
  , HE.onMouseOver $ HE.input_ $ Highlight (Index index)
  , HE.onKeyDown   $ HE.input  $ \ev -> Key ev
  , HE.onBlur      $ HE.input_ $ Blur
  ]
