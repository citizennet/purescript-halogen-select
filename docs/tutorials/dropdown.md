title: Build a PureScript Dropdown in Halogen

# Let's Build a Dropdown in PureScript!

Dropdowns are among the simplest selection components you will build, but they can be tricky to get right. For example, you'll likely want to ensure that your users can type to highlight close text matches (like when you type "Ca" to highlight "California" in a state dropdown). You'll want to be accessible to folks using screen readers or keyboard-only navigation, too. And, of course, you'll want to achieve all this without compromising on your design.

This tutorial is intended as a beginner-friendly, thorough introduction to `Select`. We'll build a functional dropdown complete with keyboard navigation. Along the way, we'll learn more about how to work with Halogen components, diagnose type errors, and other common PureScript tasks.

!!! info
    This tutorial assumes you've followed the steps in the [Project Setup](https://citizennet.github.io/purescript-halogen-select/tutorials/getting-started/) section. While not necessary, this code is tested with those steps in mind.

    It also assumes familiarity with the Halogen framework. If you need a refresher, try the official [Halogen guide](https://github.com/slamdata/purescript-halogen/tree/master/docs) or the [whirlwind tour](https://citizennet.github.io/purescript-halogen-select/tutorials/getting-started/#a-whirlwind-tour-of-our-starter-component) of our starter component.

    !!! warning ""
        **If you are already an intermediate or advanced PureScript developer, then this tutorial will read slowly for you. Feel free to skim, get the gist of how the library works, and then move on to the [faster-paced and more advanced typeahead tutorial](https://citizennet.github.io/purescript-halogen-select/tutorials/typeahead).**

    Your code should work at the end of every step. If you run into issues or your code doesn't compile, please come visit us on the [PureScript user forum](https://purescript-users.ml) or the [#fpchat Slack channel](https://functionalprogramming.slack.com).

We're going to build a dropdown that is functionally equivalent this one:

<div class="ocelot-scoped" data-component="dropdown"></div>

Our component will look a little bit worse, because we're not going to spend time on CSS.

## Basic Setup
Let's get something on the screen!

The simplest sort of dropdown has a button that can toggle a menu open or closed, a list of items that can be selected from that menu, and zero, one, or more selected items. For our dropdown we'll assume that you can select at most one item, and that selecting an item will replace the text on the button with that item.

### Rendering a button and items

We'll start by rendering the button and the items. At this point our render function contains only an empty div, so let's fill in the rest of the HTML we need:

```hs
render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
render st =
  HH.div_
  [ HH.h1_
    [ HH.text "Dropdown" ]
  , HH.button_
    [ HH.text "Click me to view some items" ]
  , HH.ul_
    [ HH.li_
      [ HH.text "Item 1" ]
    , HH.li_
      [ HH.text "Item 2" ]
    ]
  ]
```

!!! danger ""
    Make sure to compile this code and view the new output! You should see a header, a button, and two items in the list. After each step, make sure your code still compiles.


### A better `#!hs State` type

It's already clear we're going to need more than `#!hs Unit` for our `#!hs State` type. We at least need to know three things:

- If the menu is toggled on or off
- The currently-selected item (if there is one)
- The list of items available for selection

We can represent each of these with simple types in our state:

```hs
type State =
  { isOpen :: Boolean
  , selectedItem :: Maybe String
  , availableItems :: Array String
  }
```

Now that our state contains these three fields, we need to update our `#!hs initialState` function to produce the right type of values:

```hs
initialState :: Input -> State
initialState = const
  { isOpen: false
  , selectedItem: Nothing
  , availableItems:
      [ "Item One"
      , "Item Two"
      , "Item Three"
      ]
  }
```

Finally, lets update our render function to leverage the information now contained in `#!hs State`. If there's a selected item, that will be the button's text; if not, we'll fall back to a default message. If the menu is open, we'll list out the available items for selection.

For code clarity, we'll also break out the dropdown into its own helper function.

```hs
import Data.Maybe (fromMaybe)

render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
render st =
  HH.div_
  [ HH.h1_
    [ HH.text "Dropdown" ]
  , dropdown st
  ]

dropdown :: State -> H.ParentHTML Query ChildQuery ChildSlot m
dropdown =
  HH.div_
  [ HH.button_
    [ HH.text $ fromMaybe "Click me to view some items" st.selectedItem ]
  , if st.isOpen
      then HH.ul_ $ (\item -> HH.li_ [ HH.text item ]) <$> st.availableItems
      else HH.text ""
  ]
```

!!! tip
    Since the dropdown has no behavior yet, try changing the `#!hs initialState` to set `#!hs isOpen` to `#!hs true` to verify your items are in fact being rendered out to the page.

It ain't pretty, but at this point we've got all the rendering we need for a basic dropdown! The next step is to actually wire in some behavior.

## Integrating the component

Let's integrate the `Select` component! In just a few steps, we'll turn our simple render function into a fully-functioning dropdown with keyboard navigation, toggling, debounced type-to-search, and several other features.

### On building components with Select
The key idea behind the `Select` library is to **provide behaviors, not rendering**. The core component the library exposes doesn't have a render function at all! Of course, all Halogen components require a render function to work, and `Select` is no different. *You* are expected to provide that render function.

Why?

When you write the render function, not the library, you get to decide exactly what your component will look and feel like. You can also control what queries to trigger from HTML and when, effectively allowing you to control the behavior of the component *without configuration*. You can even extend it with new behavior and new state by using information from your parent component. The end result is a much smaller library component with a lot more flexibility and power for you.

We just wrote the rendering we need for an (admittedly ugly) dropdown. The render function we just wrote can actually serve almost as-is as the render function for `Select`! All we have to do is mount the `Select` component, make a few tweaks to our render code, and then pass in a little configuration information. Let's do that next.

### Importing the Select component

The first thing we'll do is bring in the `Select` library in the first place.

```hs
import Select as Select
import Select.Utils.Setters as Setters
```

!!! tip
    You can always [view the module documentation for Select on Pursuit](https://pursuit.purescript.org/packages/purescript-halogen-select/1.0.0) or the [source code on GitHub](https://github.com/citizennet/purescript-halogen-select). This is useful when integrating with third-party components so that you can check out the `#!hs Input`, `#!hs State`, `#!hs Query`, and `#!hs Message` types.

Next, we need to update our `#!hs ChildSlot` and `#!hs ChildQuery` types. We're only going to have one dropdown so we can leave the child slot as `#!hs Unit`; we do need to add the `Select` component's query type to our `#!hs ChildQuery` synonym, however.

This code, unfortunately, won't work:

```hs
type ChildQuery = Select.Query

Error found:
  Type synonym Select.Query is partially applied.
  Type synonyms must be applied to all of their type arguments.

in type synonym ChildQuery
```

The compiler has noticed that `#!hs ChildQuery`, a type synonym, now partially applied. That's because `#!hs Select.Query`, itself a type synonym, takes several arguments as described in the [module documentation on Pursuit](https://pursuit.purescript.org/packages/purescript-halogen-select/1.0.0/docs/Select#t:Query). Let's walk through each one:

```hs
type ChildQuery o item eff = Select.Query o item eff
```

`o` is *your* query type. Remember how you can embed your own queries into `Select`, and in that way extend the component's functionality? This is how. So we can fill in the first argument:

```hs
type ChildQuery item eff = Select.Query Query item eff
```

`item` is the type of whatever items you want to be selectable. Commonly these are strings, but can also be custom data types. Later on, in the [typeahead tutorial](https://citizennet.github.io/purescript-halogen-select/tutorials/typeahead), we'll see how powerful custom data types can be for rendering purposes. For our simple dropdown we'll simply specialize this to `#!hs String`:

```hs
type ChildQuery eff = Select.Query Query String eff
```

`eff` is the type of whatever effect rows your component leverages. `Select` performs some effects, like manipulating the DOM or using threads to perform debouncing on your behalf asynchronously, and it must verify that its effects match your parent component effects. We'll leave this as an argument for now, but when we mount the component, we'll provide some concrete effects.

What happens if we try to save this?

```hs
Error found in module Component :

  Could not match kind
    Type

  with kind
    # Control.Monad.Eff.Effect

while checking the kind of State -> ParentHTML Query ChildQuery ChildSlot m0
```

Whoops! Our `#!hs render` and `#!hs eval` functions expect `#!hs ChildQuery` to have the kind `#!hs Type`, but instead we've provided a type synonym that's still awaiting an argument of kind `#!hs # Effect` (read: a row of effects). We need to supply that argument. Let's update those two functions:

```hs
render :: State -> H.ParentHTML Query (ChildQuery eff) ChildSlot m
dropdown :: State -> H.ParentHTML Query (ChildQuery eff) ChildSlot m
eval :: Query ~> H.ParentDSL State Query (ChildQuery eff) ChildSlot Message m
```

Now that `Select` has been imported and we've updated our `ChildQuery` and `ChildSlot` types to support it, we can worry about what to do when we receive a message from the component.


### Mounting the component

We're finally ready to mount the `Select` component. Mounting any component in Halogen requires supplying a slot value, the component itself, the component's input, and the component's handler. We can put together all of these except for the input, which we haven't prepared yet.

Let's stub out our render function in preparation:

```hs
import Halogen.HTML.Events as HE

render :: State -> H.ParentHTML Query (ChildQuery eff) ChildSlot m
render st =
  HH.div_
  [ HH.h1_
    [ HH.text "Dropdown" ]
  , HH.slot unit Select.component ?input (HE.input <<< const Nothing)
  ]
```

Right away we get an error:

```hs
Error in module Component:

  Could not match type
    eff7

  with type
    ( dom :: DOM
    , avar :: AVAR
    | eff7
    )

when trying to match type QueryF Query String eff7
  with type QueryF Query String
```

This happened because `Select` uses the `AVAR` and `DOM` effects, but we've asserted our component will work with ANY row. That's not true anymore! Our component will now work with any row that includes `DOM` and `AVAR`. It's easy enough to fix. We need to define our own effects row, extensible by `eff`:

```hs
import Control.Monad.Aff.AVar (AVAR)
import DOM (DOM)

type Effects eff =
  ( dom :: DOM
  , avar :: AVAR
  | eff
  )
```

Now we can update our various types to use our new row, verifying the effects are the same throughout the component. As a rule of thumb, anywhere a **function** uses `eff`, wrap it in our new `Effects` type synonym, but don't apply the same rule to types or type synonyms. As an example, we'll update `component`, but we won't update the `ChildQuery` type synonym:

```hs
component :: ∀ eff m
  . MonadAff (Effects eff) m
 => H.Component HH.HTML Query Input Message m

render :: State -> H.ParentHTML Query (ChildQuery (Effects eff)) ChildSlot m
dropdown :: State -> H.ParentHTML Query (ChildQuery (Effects eff)) ChildSlot m
eval :: Query ~> H.ParentDSL State Query (ChildQuery (Effects eff)) ChildSlot Message m
```

With that out of the way, we can turn to filling in our component's input type. We can either look at the module documentation for `Select.Input` or look at the type error that resulted from our typed hole, `?input`. Both will tell us that we need to provide a value of this type:

```hs
type Input o item eff =
  { inputType     :: InputType
  , items         :: Array item
  , initialSearch :: Maybe String
  , debounceTime  :: Maybe Milliseconds
  , render        :: State item eff -> ComponentHTML o item eff
  }
```

Let's build this up, step by step. First, we see we have to provide an `#!hs InputType`. This is described in the module documentation:

```hs
-- | Text-driven inputs will operate like a normal search-driven selection component.
-- | Toggle-driven inputs will capture key streams and debounce in reverse (only notify
-- | about searches when time has expired).
data InputType
  = TextInput
  | Toggle
```

We don't have any text input for our dropdown -- its a button -- so we'll go with the `#!hs Toggle` constructor.

```hs
selectInput :: Select.Input Query String (Effects eff)
selectInput =
  { inputType: Select.Toggle
  , ...
  }
```

Next, we're expected to provide an array of items. Fortunately we already have those in our `#!hs State`. We can just send those items directly into the input.

```hs
selectInput =
  { ...
  , items: st.availableItems
  }
```

Next, we're expected to provide an initial search. This would be more useful if we had a text input, but for our dropdown, we'll start off with no initial search.

```hs
selectInput =
  { ...
  , initialSearch: Nothing
  }
```

What about a debounce time? For toggle-driven components, this is how long to aggregate key presses before the user's typing should affect the list of items. For search-driven components, this is how long to delay before raising a message with the new search. For our dropdown, we don't care:

```hs
selectInput =
  { ...
  , debounceTime: Nothing
  }
```

Finally, we're expected to provide a render function to the component. Ah ha! We've actually already written a render function for a dropdown -- it's just that the type is wrong.

### Adapting the render function for `Select`

Let's look at the types side-by-side:

```hs
Select.render :: Select.State item eff -> Select.ComponentHTML o item eff

dropdown :: State -> H.ParentHTML Query (ChildQuery (Effects eff)) ChildSlot m
dropdown st =
  HH.div_
  [ HH.button_
    [ HH.text $ fromMaybe "Click me to view some items" st.selectedItem ]
  , if st.isOpen
      then HH.ul_ $ (\item -> HH.li_ [ HH.text item ]) <$> st.availableItems
      else HH.text ""
  ]
```

From this, we can see that we need to use the state type from `Select` to drive our render function, not the state from our parent component. Will our function still work? Let's look at [`Select`'s state type in the module documentation](https://pursuit.purescript.org/packages/purescript-halogen-select/1.0.0/docs/Select#t:State) to see what we have available:

```hs
type State item eff =
  { inputType        :: InputType
  , search           :: String
  , debounceTime     :: Milliseconds
  , debouncer        :: Maybe (Debouncer eff)
  , inputElement     :: Maybe HTMLElement
  , items            :: Array item
  , visibility       :: Visibility
  , highlightedIndex :: Maybe Int
  , lastIndex        :: Int
  }
```

That's a lot of stuff! We have some of the data we need in `Select`'s state -- we have our list of items and whether the menu is open or closed. We even got new information, like which item is highlighted. But we're missing something crucial: which item is selected.

As a general rule, `Select` does not manage selections on your behalf. You are expected to decide what you want to happen when an item is selected and to store the selections yourself.

What can we do? We don't have all the information we need to write this function. Or do we?

In fact, *so long as we write the `Select` render function within the `where` clause of the parent component's render function*, we have access to the parent's state! Let's give it a shot.

```hs
render parentState =
  HH.div_
  [ HH.h1_
    [ HH.text "Dropdown" ]
  , HH.slot unit Select.component ?input (HE.input <<< const Nothing)
  ]
  where
    dropdown
      :: Select.State String (Effects eff)
      -> Select.ComponentHTML Query String (Effects eff)
    dropdown childState =
      HH.div_
      [ HH.button_
        [ HH.text $ fromMaybe "Click me to view some items" parentState.selectedItem ]
      , if childState.visibility == Select.On
          then HH.ul_ $ (\item -> HH.li_ [ HH.text item ]) <$> childState.items
          else HH.text ""
      ]
```

It works! Even better, we no longer have to manage things like `#!hs openState` in the parent anymore. Finally, now that we have the render function we need, we can finally finish our component's input type:

```hs
render :: State -> H.ParentHTML Query (ChildQuery (Effects eff)) ChildSlot m
render parentState =
  HH.div_
  [ HH.h1_
    [ HH.text "Dropdown" ]
  , HH.slot unit Select.component selectInput (HE.input <<< const Nothing)
  ]
  where
    selectInput :: Select.Input Query String (Effects eff)
    selectInput =
      { inputType: Select.Toggle
      , items: parentState.availableItems
      , initialSearch: Nothing
      , debounceTime: Nothing
      , render: dropdown
      }

    dropdown = ...
```

## Integrating Behavior

Everything up to this point has been standard Halogen except for writing the child component's render function. At this point, the `Select` component is running -- good work! However, it's not yet *doing* anything.

It's now time to turn your static HTML into a fully-functioning dropdown.

### Attaching behavior to Select

`Select` works by using a few helper functions that attach at critical points in your render function. The library assumes very little about what your rendering looks like, except that there _at least_ exists:

- One or more items that can be selected
- An element that contains those items
- A focusable element that can be used to toggle visibility and capture keystrokes

Accordingly, you'll need to use three helper functions, each exported by the `Select.Utils.Setters` module:

- `setItemProps`
- `setContainerProps`
- `setToggleProps` (for toggle-driven input)
- `setInputProps` (for text-driven input)

Each of these functions should be used on the property array for the relevant element in your HTML. Let's walk through each one using our built render function.

First, let's augment our individual items. `#!hs setItemProps` takes an index and some properties and outputs some new properties, which include all sorts of event handlers necessary for keyboard events and click events to work. In order to provide it with the index it needs, we'll use the `#!hs mapWithIndex` function from `#!hs Data.Array`.

```hs
import Data.Array (mapWithIndex)

dropdown childState =
  HH.div_
  [ HH.button_
    [ HH.text $ fromMaybe "Click me to view some items" parentState.selectedItem ]
  , case childState.visibility of
      Select.Off -> HH.text ""

      Select.On -> HH.ul [] $
        mapWithIndex
          (\ix item -> HH.li (Setters.setItemProps ix []) [ HH.text item ])
          childState.items
  ]
```

Next, we'll move to the element that contains the items. The `#!hs setContainerProps` function takes and returns some properties, attaching all the behavior the library needs. We'll use this on the parent element, `ul`:

```hs
dropdown childState =
  HH.div_
  [ HH.button_
    [ HH.text $ fromMaybe "Click me to view some items" parentState.selectedItem ]
  , case childState.visibility of
      Select.Off -> HH.text ""

      Select.On -> HH.ul (Setters.setContainerProps []) $
        mapWithIndex
          (\ix item -> HH.li (Setters.setItemProps ix []) [ HH.text item ])
          childState.items
  ]
```

Finally, we can make sure that our button toggles the menu on and off, captures keyboard events, can be tabbed to, and all sorts of other stuff with the `#!hs setToggleProps` function.

```hs
dropdown childState =
  HH.div_
  [ HH.button
    (Setters.setToggleProps [])
    [ HH.text $ fromMaybe "Click me to view some items" parentState.selectedItem ]
  , case childState.visibility of
      Select.Off -> HH.text ""

      Select.On -> HH.ul [] $
        mapWithIndex
          (\ix item -> HH.li (Setters.setItemProps ix []) [ HH.text item ])
          childState.items
  ]
```

Whew! Your rendering code now contains everything it needs to provide a keyboard-accessible dropdown. If you open this up in the browser and click around, you'll notice it's properly toggling and can be tabbed to.

Let's make one last improvement. When you use your arrow keys on the dropdown, the highlighted index is changing, but since we didn't provide any CSS we can't see it. Let's add some bare-bones styling so we can watch the highlights:

```hs
import Halogen.HTML.Properties as HP

Select.On -> HH.ul (Setters.setContainerProps []) $
  mapWithIndex
    (\ix item ->
      HH.li
        ( Setters.setItemProps ix
          $ case Just ix == childState.highlightedIndex of
              true -> [ HP.attr (HH.AttrName "style") "color: red;" ]
              _ -> [] )
        [ HH.text item ]
    )
    childState.items
```

There we go! Try toggling the menu on and off, using your arrow, enter, and escape keys, and so on. It works!

...almost. Alas, we aren't doing anything when the user makes a selection. `Select` is attempting to notify us that a selection occurred, but we never provided a handler. Let's fix that now.

### Handling messages from Select

When you add a new child component you invariably need to add a handler for its `#!hs Message` type. What should the parent do when something important has occurred in the child? To handle messages, add a new constructor to your query algebra that takes the child's `#!hs Message` type as an argument:

```hs
data Query a
  = NoOp a
  | HandleSelect Select.Message a
```

Ah -- this won't compile!

```hs
Error found in module Component:

  Could not match kind
    (Type -> Type) -> Type -> Type

  with kind
    Type

in type constructor Query
```

This looks similar to the type error we got when we tried to just use `Select.Query` in a type synonym. We need to provide a `#!hs Type` to `#!hs HandleSelect`, but `#!hs Select.Message` is still awaiting 2 arguments, the first of which is *itself* awaiting an argument! Let's go look at the [module documentation for `Select.Message`](https://pursuit.purescript.org/packages/purescript-halogen-select/1.0.0/docs/Select#t:Message).

```hs
data Message o item
```

We've seen both of these arguments before in the component's query type, so we should fill them in with the same values. `o` is our parent component query type, and `item` is a `#!hs String`:

```hs
data Query a
  = NoOp a
  | HandleSelect (Select.Message Query String) a
```

As soon as you save and rebuild you'll see another compiler error!

```hs
Error found in module Component

  A case expression could not be determined to cover all inputs.
  The following additional cases are required to cover all inputs:

    (HandleSelect _ _)

in value declaration component
```

This time it's because we've added a new query, but we never updated our `#!hs eval` function to describe what should happen when the query is triggered. What should we actually _do_ when a message comes up from the child component?

!!! tip
    You'll often see type errors that end in "... value declaration component." when the error occurred in any of the functions in the `where` clause for the component. It can be annoying to track down where the error actually is in your code. One way to help track these down is to move your code out of the `where` block and into the module level temporarily so the compiler can identify which particular function is causing the issue.

There are four possible sub-cases that we need to handle, each described in the module documentation:

```hs
data Message o item
  = Searched String
  | Selected item
  | VisibilityChanged Visibility
  | Emit (o Unit)
```

Let's stub out each of these cases and then decide what to do with them:

```hs
eval :: Query ~> H.ParentDSL State Query (ChildQuery eff) ChildSlot Message m
eval = case _ of
  NoOp next -> pure next

  HandleSelect message next -> case message of
    Select.Searched string ->
      pure next

    Select.Selected item ->
      pure next

    Select.VisibilityChanged vis ->
      pure next

    Select.Emit query ->
      pure next
```

Let's take these case-by-case.

What should we do when the user has searched something on our dropdown? This is just a simple list of items, so we'll simply ignore their search. We can leave this as `#!hs pure next`.

What should we do when the user has selected an item? Ah! This is more interesting. We want to set their item as the currently-selected one, and then we want to remove it from the available items list. Once we've removed it from the available items, we'll update `Select` with its new items to display and we'll toggle it off.

We can use `#!hs difference` from `#!hs Data.Array` to filter out the selected item from the overall list of items. This is a common pattern in `Select`: the parent holds the immutable list of all possible items, and `Select` receives some subset of those items at each render. You might use the user's search to filter out items in a typeahead, for example, or only load 50 results at a time into a dropdown.

```hs
import Data.Array (difference)

Select.Selected item -> do
  st <- H.get
  _ <- H.query unit $ Select.setVisibility Select.Off
  _ <- H.query unit $ Select.replaceItems $ difference st.availableItems [ item ]
  H.modify _ { selectedItem = Just item }
```

What should we do when the dropdown's visibility has changed? This can often be useful to run validation, but for our dropdown, we don't care what its visibility is. We can leave this as `#!hs pure next`.

Finally, what should we do when the child component raises its `Emit` message? What does this even mean? `Emit` exists so you can embed your own queries into `Select` and extend its behavior. Since the message contains one of your own queries, all you have to do is evaluate it: you can call `eval` recursively to run your query.

You can think of `Emit` as notifying you that the query you embedded is ready to run.

```hs
Select.Emit query -> do
  eval query
  pure next
```

Nice and simple! While you may write all kinds of logic for the other messages raised by `Select`, you'll always write this same code for the `Emit` message.

## Conclusion

Congratulations! You have successfully built a keyboard-navigable dropdown using `Select`. You integrated the library, wrote your own render function, and then augmented it with helper functions from the library. Then, you handled the output messages and sent queries to update the component's state. You've done quite a lot of work!

!!! tip
    Did you notice anything you would improve about this tutorial or the `Select` library? I'd love to hear about it! Feel free to reach out on the [functional programming Slack](https://functionalprogramming.slack.com/) or on the [PureScript user forum](https://purescript-users.ml). If you found a bug or would like to make an improvement, please open an issue or pull request on the library.

### Next Steps

This tutorial was a slow, thorough introduction to the `Select` library. But we've only scratched the surface of what you can do with it. I'd recommend continuing on to the faster-paced and more advanced [typeahead tutorial](https://citizennet.github.io/purescript-halogen-select/tutorials/typeahead).

### Source Code

If you'd like to use this component as a starting point from which to build your own, feel free to copy/paste the source code below.

??? article "Full source code for the tutorial"
    ```hs
    module Component where

    import Prelude

    import Control.Monad.Aff.AVar (AVAR)
    import Control.Monad.Aff.Class (class MonadAff)
    import DOM (DOM)
    import Data.Array (difference, mapWithIndex)
    import Data.Maybe (Maybe(..), fromMaybe)
    import Halogen as H
    import Halogen.HTML as HH
    import Halogen.HTML.Events as HE
    import Halogen.HTML.Properties (attr) as HP
    import Select as Select
    import Select.Utils.Setters as Setters

    data Query a
      = HandleSelect (Select.Message Query String) a

    type State =
      { selectedItem :: Maybe String
      , availableItems :: Array String
      }

    type Input = Unit

    type Message = Void

    type ChildSlot = Unit
    type ChildQuery eff = Select.Query Query String eff

    type Effects eff =
      ( dom :: DOM
      , avar :: AVAR
      | eff
      )

    component :: ∀ eff m
      . MonadAff (Effects eff) m
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
      initialState = const
        { isOpen: false
        , selectedItem: Nothing
        , availableItems:
            [ "Item One"
            , "Item Two"
            , "Item Three"
            ]
        }

      render :: State -> H.ParentHTML Query (ChildQuery (Effects eff)) ChildSlot m
      render parentState =
        HH.div_
        [ HH.h1_
          [ HH.text "Dropdown" ]
        , HH.slot unit Select.component selectInput (HE.input HandleSelect)
        ]
        where
          selectInput :: Select.Input Query String (Effects eff)
          selectInput =
            { inputType: Select.Toggle
            , items: parentState.availableItems
            , initialSearch: Nothing
            , debounceTime: Nothing
            , render: dropdown
            }

          dropdown
            :: Select.State String (Effects eff)
            -> Select.ComponentHTML Query String (Effects eff)
          dropdown childState =
            HH.div_
            [ HH.button
              (Setters.setToggleProps [])
              [ HH.text $ fromMaybe "Click me to view some items" parentState.selectedItem ]
            , case childState.visibility of
                Select.Off -> HH.text ""

                Select.On -> HH.ul (Setters.setContainerProps []) $
                  mapWithIndex
                    (\ix item ->
                      HH.li
                        ( Setters.setItemProps ix
                          $ case Just ix == childState.highlightedIndex of
                              true -> [ HP.attr (HH.AttrName "style") "color: red;" ]
                              _ -> [] )
                        [ HH.text item ]
                    )
                    childState.items
            ]

      eval :: Query ~> H.ParentDSL State Query (ChildQuery (Effects eff)) ChildSlot Message m
      eval = case _ of
        HandleSelect message next -> case message of
          Select.Searched string ->
            pure next

          Select.Selected item -> do
            st <- H.get
            _ <- H.query unit $ Select.setVisibility Select.Off
            _ <- H.query unit $ Select.replaceItems $ difference st.availableItems [ item ]
            H.modify _ { selectedItem = Just item }
            pure next

          Select.VisibilityChanged vis ->
            pure next

          Select.Emit query -> do
            eval query
            pure next
    ```
