## Module Select.Dispatch

#### `Dispatch`

``` purescript
data Dispatch item o e a
  = ParentQuery (o Unit) a
  | Search (SearchQuery item o e) a
  | Container (ContainerQuery item o e) a
```

The `Dispatch` type serves as the query type for all primitives
and has data constructors for handling queries of the parent's type
or wrapping the queries of the primitives. When you use any primitive,
you will supply the Dispatch type as the primitive's query.
See the respective primitives for their available queries.

`item` - Your defined type of the items you are storing in the primitives
`o` - The query type of the parent component containing the primitives
`e` - The effects type of the primitives
`a` - The Halogen return type, which must remain polymorphic as in all Halogen components.

#### `State`

``` purescript
type State s item o e = Store s (ComponentHTML (Dispatch item o e))
```

All primitives use the `Store` type as their component state. Any additional
data, like a traditional Halogen State record, will usually be provided.

Note: It's necessary to use a `Store` comonad as the state type for primitives
because they receive their render function as input. The render function cannot
be stored in the State type synonym due to cycles, and as your component render
function can only take `State` as its input, you need the render function available
via the comonad, StoreT.

`s` - The State type defined for the primitive.

#### `SearchQuery`

``` purescript
data SearchQuery item o e
  = TextInput String
  | SearchReceiver (SearchInput item o e)
```

The query type for the `Search` primitive. This primitive handles text input
and debouncing.

TextInput - handle new text input as a string
SearchReceiver - update the component with new `Input` when the parent re-renders.

#### `SearchState`

``` purescript
type SearchState e = { search :: String, ms :: Milliseconds, debouncer :: Maybe (Debouncer e) }
```

The `Search` primitive internal state
- `search`: the `String` contained within the primitive
- `ms`: number of milliseconds for the input to be debounced before passing
        a message to the parent. Set to 0.0 if you don't want debouncing.
- `debouncer`: used to facilitate debouncing of the input

#### `Debouncer`

``` purescript
type Debouncer e = { var :: AVar Unit, fiber :: Fiber (Effects e) Unit }
```

The `Debouncer` type alias, used to debounce user input in the `Search` primitive.

#### `SearchInput`

``` purescript
type SearchInput item o e = { search :: Maybe String, debounceTime :: Milliseconds, render :: SearchState e -> ComponentHTML (Dispatch item o e) }
```

The input type of the `Search` primitive
- `search`: an optional initial value for the `search` key on the `SearchState`
- `debounceTime`: a value in milliseconds for the debounce delay. Set to 0.0 for
no debouncing.
- `render`: the render function for the primitive

#### `ContainerQuery`

``` purescript
data ContainerQuery item o e
  = Highlight Target
  | Select Int
  | Key KeyboardEvent
  | Mouse MouseState
  | Blur
  | Visibility VisibilityStatus
  | ContainerReceiver (ContainerInput item o e)
```

The query type for the `Container` primitive.

Highlight - change the highlighted item to the next, previous, or a specific index.
Select - select an item at the specified index
Key - capture key events for arrow navigation, Escape to close, and Enter to select.
Mouse - capture mouse events to close the menu or select an item
Blur - trigger the DOM blur event
Visibility - set the visibility by toggling, setting to on, or setting to off.
ContainerReceiver - update the component on new `Input` when the parent re-renders.

#### `Target`

``` purescript
data Target
  = Next
  | Prev
  | Index Int
```

For targeting items in the container via the `ContainerQuery`'s `Highlight` constructor

#### `MouseState`

``` purescript
data MouseState
  = Down
  | Up
```

For maintaining the state of the mouse in the `Container`

#### `VisibilityStatus`

``` purescript
data VisibilityStatus
  = On
  | Off
  | Toggle
```

For showing or hiding the `Container`

#### `ContainerState`

``` purescript
type ContainerState item = { items :: Array item, open :: Boolean, highlightedIndex :: Maybe Int, lastIndex :: Int, mouseDown :: Boolean }
```

The internal state of the `Container` primitive
- `items`: an array of items held within the `Container`
- `open`: whether the `Container` is visible
- `highlightedIndex`: the index of the highlighted item, if one exists
- `lastIndex`: the index of the last item in the `Container`
- `mouseDown`: whether the mouse is clicked or not

#### `ContainerInput`

``` purescript
type ContainerInput item o e = { items :: Array item, render :: ContainerState item -> ComponentHTML (Dispatch item o e) }
```

The input type of the `Container` primitive
- `items`: the initial value of `items` in the `ContainerState`
- `render`: the `render` function for the `Container` primitive

#### `emit`

``` purescript
emit :: forall a0 a1 o item e f. Applicative f => (o Unit -> f Unit) -> Dispatch item o e a0 -> a1 -> f a1
```

A helper function for conveniently evaluating a `ParentQuery` using the parent's `eval` function

It accepts an `eval` function, a `Dispatch` query, and the `a` from the parent's context.

WARNING: This function should only be used when you truly want to ignore all emitted queries except
for your own parent component queries. If you need to route queries among primitives (for example, route
a key press on a search primitive to a key event on a container primitive), then you will need to manually
route them. See the documentation for more details.

#### `updateStore`

``` purescript
updateStore :: forall state html. (state -> html) -> (state -> state) -> Store state html -> Store state html
```

Helper for wholly updating the `State` (`Store`) of a primitive

Used when the `render` function needs to be updated
Note: Use `seeks` if only the primitive's internal state needs to be updated (not the entire Store).

#### `getState`

``` purescript
getState :: forall m s a. MonadState (Store s a) m => m (Tuple (s -> a) s)
```

Helper to get and unpack the primitive state type from the Store type. When used with pattern matching,
you can access state with:
(Tuple renderFunction state) <- getState

#### `augmentHTML`

``` purescript
augmentHTML :: forall props q. Array (IProp props q) -> Array (IProp props q) -> Array (IProp props q)
```

Helper used to concatenate two `Array IProps`, assuring that our events overwrite
the user's when they conflict.

For allowing the user to include arbitrary properties
on the elements in the render function passed to the primitive.

#### `embed`

``` purescript
embed :: forall item parentQuery e. Action parentQuery -> Unit -> Dispatch item parentQuery e Unit
```

Embed a parent query into a `Dispatch` type. In use:
[ onClick $ input_ $ embed YourQueryType ]

#### `getInputProps`

``` purescript
getInputProps :: forall item o e p. Array (IProp (onFocus :: FocusEvent, onKeyDown :: KeyboardEvent, onInput :: Event, value :: String, onMouseDown :: MouseEvent, onMouseUp :: MouseEvent, onBlur :: FocusEvent, tabIndex :: Int | p) (Dispatch item o e)) -> Array (IProp (onFocus :: FocusEvent, onKeyDown :: KeyboardEvent, onInput :: Event, value :: String, onMouseDown :: MouseEvent, onMouseUp :: MouseEvent, onBlur :: FocusEvent, tabIndex :: Int | p) (Dispatch item o e))
```

Intended for use on the text input field with the Search primitive. If you are using a button
to toggle the menu instead, use the `getToggleProps` helper.

#### `getToggleProps`

``` purescript
getToggleProps :: forall item o e p. Array (IProp (onClick :: MouseEvent, onKeyDown :: KeyboardEvent, onMouseDown :: MouseEvent, onMouseUp :: MouseEvent, onBlur :: FocusEvent, tabIndex :: Int | p) (Dispatch item o e)) -> Array (IProp (onClick :: MouseEvent, onKeyDown :: KeyboardEvent, onMouseDown :: MouseEvent, onMouseUp :: MouseEvent, onBlur :: FocusEvent, tabIndex :: Int | p) (Dispatch item o e))
```

Intended for use on a clickable toggle to show/hide the `Container` primitive. If
you are using a text field to manage the container, use `getInputProps` instead.

#### `getContainerProps`

``` purescript
getContainerProps :: forall item o e p. Array (IProp (onMouseDown :: MouseEvent, onMouseUp :: MouseEvent, onBlur :: FocusEvent, tabIndex :: Int | p) (Dispatch item o e)) -> Array (IProp (onMouseDown :: MouseEvent, onMouseUp :: MouseEvent, onBlur :: FocusEvent, tabIndex :: Int | p) (Dispatch item o e))
```

Intended to be used on the container primitive to capture key, click, highlighting, and other events

#### `getChildProps`

``` purescript
getChildProps :: forall item o e p. Array (IProp (onBlur :: FocusEvent, tabIndex :: Int | p) (Dispatch item o e)) -> Array (IProp (onBlur :: FocusEvent, tabIndex :: Int | p) (Dispatch item o e))
```

Intended for anything that will be embedded into the container primitive. For example, if you embed
a button with your own functionality into the container primitive, you might do this:
button ( getChildProps [ onClick $ input_ $ embed YourQueryType ] ) [ text "Button text" ]

#### `getItemProps`

``` purescript
getItemProps :: forall item o e p. Int -> Array (IProp (onClick :: MouseEvent, onMouseOver :: MouseEvent, onKeyDown :: KeyboardEvent, onBlur :: FocusEvent, tabIndex :: Int | p) (Dispatch item o e)) -> Array (IProp (onClick :: MouseEvent, onMouseOver :: MouseEvent, onKeyDown :: KeyboardEvent, onBlur :: FocusEvent, tabIndex :: Int | p) (Dispatch item o e))
```

Intended for use on the element rendered for each item in the `Container`.


