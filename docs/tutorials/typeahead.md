title: Build a PureScript Typeahead (Autocomplete) in Halogen

# Let's Build a Typeahead in PureScript!

Typeaheads are among the most common selection components you'll build. Most web developers have had to implement at least one of these before and they can be surprisingly difficult to build. Luckily, with `Select`, implementing a typeahead that fits your custom design takes little more than writing the rendering code and then tweaking it with a helper function or two.

In this tutorial we'll build a typeahead with the following features:

- Users can search Star Wars characters by name; their searches will be debounced automatically and results will be fetched asynchronously.
- The typeahead should support keyboard-only use: arrow keys should step up and down the items, Enter should select, Escape should close, and so on.
- The typeahead should manage its own selections, including insertion and removal, and should notify its parent when the selections have changed.
- If a search returns no results, then there should be an embedded "fetch data" button the user can click to force a request with an empty search. It should display within the list of items.

Along the way, we'll see how to extend `Select`'s features by embedding parent queries (we'll use this to embed the "fetch data" button in the list).

!!! info
    This tutorial assumes you've followed the steps in the [Project Setup](https://citizennet.github.io/purescript-halogen-select/tutorials/getting-started/) section. While not necessary, this code is tested with those steps in mind.

    It also assumes familiarity with Halogen and intermediate PureScript experience or that you have already completed the [more thorough, beginner-friendly dropdown tutorial](https://citizennet.github.io/purescript-halogen-select/tutorials/dropdown). If you need a Halogen refresher, try the official [Halogen guide](https://github.com/slamdata/purescript-halogen/tree/master/docs) or the [whirlwind tour](https://citizennet.github.io/purescript-halogen-select/tutorials/getting-started/#a-whirlwind-tour-of-our-starter-component) of our starter component.

    Your code should work at the end of every step. If you run into issues or your code doesn't compile, please come visit us on the [PureScript user forum](https://discourse.purescript.org) or the [#fpchat Slack channel](https://functionalprogramming.slack.com).


## Basic Setup

In this tutorial, we'll build a typeahead component from scratch. You can either follow along using the minimal component from the [Project Setup section](https://github.citizennet.io/purescript-halogen-select/tutorials/getting-started) or start your own.

If you didn't follow the project setup, grab the source for our starting component here:

??? article "Source code for a minimal starting component"
    ```hs
    module Component where

    import Prelude

    import Effect.Aff.Class (class MonadAff)
    import Data.Const (Const)
    import Data.Maybe (Maybe(..))
    import Halogen as H
    import Halogen.HTML as HH

    data Query a
      = NoOp a

    type State = Unit
    type Input = Unit

    type Message = Void

    type ChildSlot = Unit
    type ChildQuery = Const Void

    component :: ∀ m. MonadAff m => H.Component HH.HTML Query Input Message m
    component =
      H.parentComponent
        { initialState
        , render
        , eval
        , receiver: const Nothing
        }
      where

      initialState :: Input -> State
      initialState = const unit

      render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
      render st = HH.div_ []

      eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message m
      eval = case _ of
        NoOp next -> pure next
    ```

### Install dependencies

The first thing we'll do is make sure we have the libraries we need installed. Our typeahead is going to make API calls on our behalf, decode the response, and keep track of the state of requests using a special `#!hs RemoteData` data type. Let's go ahead and install our dependencies:

```shell
# These should already be installed as part of the project setup
bower i --save purescript-halogen purescript-halogen-select purescript-affjax

# These are new dependencies
bower i --save purescript-argonaut purescript-remotedata

# Let's compile the new dependencies to ensure they're available to import
yarn build
```

### Integrate the component

Now let's make sure we have `Select` ready to go in our component. Import the library:

```hs
import Select as Select
import Select.Setters as Setters
```

Next, since `Select` is going to be a child component, we'll need to update several types and functions. We will:

- Delete the unnecessary `#!hs NoOp` query and relevant case in `#!hs eval`
- Add a new query to handle messages emitted by `Select`
- Update our `#!hs ChildQuery` type synonym to contain `Select`'s query type
- Update the type signatures for `#!hs eval` and `#!hs render` with the new `#!hs ChildQuery`
- Add a new case to `#!hs eval` for our new `#!hs HandleSelect` query

!!! tip
    This tutorial doesn't explain things like child queries, slots, inputs, rendering, `#!hs Free`, `#!hs eval` functions, or other crucial Halogen knowledge. If you feel lost, I'd recommend checking out the [dropdown tutorial](https://citizennet.github.io/purescript-halogen-select/tutorials/dropdown) before continuing.

Of course, we won't be prepared to handle messages or use `Select`'s queries without knowing what they are. Let's start with the query type for the `Select` component, `#!hs QueryF`:

```hs
-- | - `o`: The query type of the component that will mount this component in a child slot.
-- |        This allows you to embed your own queries into the `Select` component.
-- | - `item`: Your custom item type. It can be a simple type like `String`, or something
-- |           complex like `CalendarItem StartDate EndDate (Maybe Disabled)`.
data QueryF o item a
  = Search String a
  | Highlight Target a
  | Select Int a
  | Focus Boolean a
  | Key KE.KeyboardEvent a
  | PreventClick ME.MouseEvent a
  | SetVisibility Visibility a
  | GetVisibility (Visibility -> a)
  | ReplaceItems (Array item) a
  | Raise (o Unit) a
  | Receive (Input o item) a
```

Already we're faced with an interesting decision: how should we fill in the type variables that `Select` expects?

- `o` represents the type of queries that can be embedded in the component. You should fill this in with your parent component's query type. If you follow Halogen convention and name your type `#!hs Query`, then filling this variable in will produce `#!hs Select.Query Query item eff`. If you take a look at where this variable is used, you'll see it shows up in the `Select` component's `#!hs Raise` and `#!hs Receive` queries. The `#!hs Raise` query is a wrapper that you can use to embed your query into the render function you provide to the component. The `#!hs Receive` query leverages `Select`'s `#!hs Input` type, which includes that render function. I'll have a lot more to say about embedding your own query type into `Select` later on.

The second type argument is more interesting. `Select` allows you to provide any type as your selectable "item". While in this tutorial we're going to stick with strings you could very well make a significantly more information-rich type.

??? tip "Writing useful item types"
    Any time you need to render some items differently than others, or you need different logic for when one item is selected vs. another, you should encode that information in the item type. For example, at CitizenNet, our calendar component has an item type like this:

    ```hs
    data CalendarItem = CalendarItem SelectedStatus DisabledStatus Boundary Range Date
    ```

    These custom types give us everything we need to know to render various dates and handle them when selected. For example, if you want some items to be selectable and others to be disabled, you could create an item type like this:

    ```hs
    data Item = Selectable String | Disabled String

    renderItem ix (Selectable str)
      = HH.li ( Setters.setItemProps ix [ ] ) [ HH.text str ]
    renderItem _ (Disabled str)
      = HH.li_  [ HH.text str ]
    ```

With all this information in mind, let's go ahead and make those changes:

```hs
data Query a
  = HandleSelect (Select.Message Query String) a

type ChildSlot = Unit
type ChildQuery = Select.Query Query String

component =
  ...
  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render st = HH.div_ []

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message m
  eval = case _ of
    -- We'll just stub this out for the time being.
    HandleSelect message next -> pure next
```

Next, we'll actually mount the `Select` component. We have everything except for the component's `#!hs Input` type so far, so we'll fill that in and leave the input as a type hole.

```hs
import Halogen.HTML.Events as HE

render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
render st =
	HH.div_
	[ HH.slot unit Select.component ?input (HE.input HandleSelect) ]
```

With that out of the way, we can turn to the component's input type. Here's what we're required to fill in, as per the [`Select` module documentation](https://pursuit.purescript.org/packages/purescript-halogen-select/3.0.0/docs/Select#t:Input):

```hs
-- | Text-driven inputs will operate like a normal search-driven selection component.
-- | Toggle-driven inputs will capture key streams and debounce in reverse (only notify
-- | about searches when time has expired).
data InputType
  = TextInput
  | Toggle

-- | The component's input type, which includes the component's render function. This
-- | render function can also be used to share data with the parent component, as every
-- | time the parent re-renders, the render function will refresh in `Select`.
type Input o item =
  { inputType     :: InputType
  , items         :: Array item
  , initialSearch :: Maybe String
  , debounceTime  :: Maybe Milliseconds
  , render        :: State item -> ComponentHTML o item
  }
```

Let's look at these one-by-one:

1. We're using an input field in the DOM, so we'll use the `#!hs TextInput` type to drive the component.
2. We don't have any items yet (they'll be fetched via the Star Wars API), so we'll provide an empty array.
3. We don't want there to be an initial search; we'll wait for the user to type something. However, if at any point we want to fill in text in the input field (for example, set the text to the full selection when the user selects something), we can use this field to accomplish that.
4. We're making API calls every time the user performs a search, so we'll set a reasonable debounce time of a few hundred milliseconds.
5. Ah, the big issue: we need to write a render function and pass it in to the component. We don't have one yet, so we'll stub this out with a simple empty `#!hs div`.

Let's write that input record now:

```hs
import Data.Time.Duration (Milliseconds(..))

render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
render st =
  HH.div_
  [ HH.slot unit Select.component selectInput (HE.input HandleSelect) ]

selectInput :: Select.Input Query String
selectInput =
  { inputType: Select.TextInput
  , items: []
  , initialSearch: Nothing
  , debounceTime: Just $ Milliseconds 250.0
  , render: \_ -> HH.div_ [ HH.text "Not implemented" ]
  }
```

All right! We've fully integrated the `Select` component. It's a little tedious to integrate the component the first time you do it, but it soon becomes second nature. At this point, we're ready to start writing our typeahead.

!!! danger ""
    Now would be a good time to verify that this component is rendering properly. Compile the project and point your browser to `dist/index.html`. You should see text rendering from within `Select`.

## A minimal typeahead

Let's take a step back now that we have `Select` integrated. We are building a typeahead that will fetch some data asynchronously when the user makes a search. It needs to maintain a list of items that can be selected, and a list of items that have already been selected. The user should only be able to select any item once, so these two lists should have no shared items. We'd like the typeahead to handle all the data fetching and selections behind the scenes, and only notify the parent component when the selections have been updated.

With this information in mind, we can step through the key data types in our Halogen component and ensure they accurately capture the features we want.

!!! note
    In the dropdown tutorial, we started by writing a render function and only later worried about state, queries, messages, and so on. However, I usually like to work in the other direction. We already know the behaviors and data we need to manage, and we don't need to render anything to implement them, though we'll certainly use our rendered component for testing.

    Instead, we'll work through the major data types in our component and only once those are completed will we write some minimal rendering code. It might feel a little strange to spend so much time on data without once touching the HTML, but by the time we reach our rendering function it will naturally extend from the data.


### State

From our requirements, we know we'll need some information:

- A list of items that can be selected by the user
- A list of items that have already been selected, and which can be removed
- The user's last search, so we can use it to fetch new data from the Star Wars API

It'll also be nice to have some extra information purely for rendering purposes, like:

- An indicator as to whether the menu should be displayed or not
- An indicator as to which item the user has focused, so we can highlight it

We have access to two distinct `#!hs State` types when we use `Select`: the parent component state, which we own, and the `Select` component's state, which we can read and write. There's no point in duplicating information between the two if we can help it. But we have access to even more information: messages output by the component. Sometimes we can simply rely on the contents of these messages to take action without ever storing the result in state.

Let's take a quick look at what `Select` provides (take a look at the [module documentation](https://pursuit.purescript.org/packages/purescript-halogen-select/1.0.0/docs/Select#t:State) for more details):

```hs
type State item =
  { inputType        :: InputType
  , search           :: String
  , debounceTime     :: Milliseconds
  , debouncer        :: Maybe Debouncer
  , inputElement     :: Maybe HTMLElement
  , items            :: Array item
  , visibility       :: Visibility
  , highlightedIndex :: Maybe Int
  , lastIndex        :: Int
  }

data Message o item
  = Searched String
  | Selected item
  | VisibilityChanged Visibility
  | Emit (o Unit)
```

It looks like we already have the list of selectable items stored in `Select`, so we don't need that in our component state. We're fetching new items via an external API, so after each new search we can simply pass the new items straight down.

We also have the user's search stored in `State` and also raised as a message every time the debouncer runs out. We don't really care about every keystroke the user types, so we'll rely on the `#!hs Searched` message for this information.

We have our two pieces of rendering information, too, with the `#!hs visibility` and `#!hs highlightedIndex` fields.

In fact, it looks like the only thing we have to store in our `#!hs State` is the list of selecetions! That keeps things simple.

```hs
type State = { selections :: Array String }
```

!!! tip
    `Select` doesn't manage any selections on your behalf. What should happen when an item gets selected, after all? In some cases, you might want to stick it into a "selected" list and remove it from the list of available options. In others, you might want it to be selectable multiple times. Or you might want to just apply a highlight, like in a calendar picker. Rather than force you to fill out a configuration record, `Select` defers the decision to you.

This is OK, but I'd like some more information. We're fetching data asynchronously, right? That means that requests could possibly fail, or they might be in progress for a long time, or perhaps they might never get triggered in the first place. Ideally our typeahead could render differently depending on these states. If we don't keep track of our requests in `#!hs State`, we won't have any of this information available for rendering.

It's the same idea as using an information-rich custom `item` type to add nuance to your rendering code. Luckily, there already exists a lovely package named `purescript-remotedata` that supplies us with a data type we can use to model each of these states:

```hs
data RemoteData e a
  = NotAsked
  | Loading
  | Failure e
  | Success a
```

So while it's not strictly necessary to maintain a list of items in our state, we'll leverage `#!hs RemoteData` to have a more useful state type.

```hs
type State =
  { items :: RemoteData String (Array String)
  , selections :: Array String
  }

initialState :: Input -> State
initialState = const
  { items: NotAsked
  , selections: []
  }
```

### Query

Now that we've got a usable `#!hs State` type, let's turn to our queries. Queries are the computations available to the component, so they're the place where we ought to think about what the typeahead should *do*, rather than just how it should render.
3

`Select` is going to manage all the keyboard events, text input, debouncing, moving the highlighted index, and so on. On top of that, we'll need to add some extra functionality: the ability to remove items that have already been selected, and the ability to fetch new items when the user performs a search. We'll at least need two queries to handle these two features.

Luckily, though, we already _have_ a query available for when a new search has been performed: our `#!hs HandleSelect` query tied to the `#!hs Select.Searched` message! That means we really only need one new query:

```hs
data Query a
  = HandleSelect (Select.Message Query String) a
  | Remove String a

eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message m
eval = case _ of
  HandleSelect message next -> case message of
    Select.Searched str ->
      pure next
    Select.Selected item ->
      pure next
    Select.VisibilityChanged vis ->
      pure next
    Select.Emit query ->
      pure next

  Remove item next ->
    pure next
```

What do we want to happen in each of these queries? Let's work from the bottom to the top.

#### Remove

When the user clicks on an item that is already selected, we want to remove it from the selected list. We also want to re-insert it into the available items in `Select`. It's easy enough to accomplish this:

```hs
Remove item next -> do
  H.modify \st -> st { selections = filter (_ /= item) st.selections }
  st <- H.get
  _ <- H.query unit
    $ Select.replaceItems
    $ difference (withDefault [] st.items) st.selections
  pure next
```

#### Emit

What should we do when we get the `#!hs Emit` message? This is returning our own query to us so we can run it, so we can recursively call `#!hs eval` with the query. You'll use this pattern every time you implement a new component with `Select`:

```hs
Select.Emit query -> eval query *> pure next
```

#### VisibilityChange

What about when the visibility changes? We don't actually care about this one, so we'll ignore it. It's useful for validation, if we were to implement that.


#### Selected

What about when an item is selected? This one is like the inverse of our `#!hs Remove` query. We want to remove the item from the available items and add it to the list of selections.

```hs
Select.Selected item -> do
  H.modify \st -> st { selections = item : st.selections }
  st <- H.get
  _ <- H.query unit
    $ Select.replaceItems
    $ difference (withDefault [] st.items) st.selections
  pure next
```

#### Searched

We can finally consider what to do when the user performs a search. We won't do any fancy filtering on our own; we're going to punt that responsibility to an external API. Still, now we have to write the code to fetch that data.

Our function will hit the Star Wars API, decode the result into an array of strings, and then return them. In the case of failure, we'll return an error message.

```hs
import Data.Argonaut (Json, decodeJson, (.?))
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response as Response

fetchItems :: String -> Aff (Either String (Array String))
fetchItems str = do
  res <- _.response <$>
    ( AX.get
    $ Response.json
    $ "https://swapi.co/api/people/?search=" <> str
    )

  pure $ do
    obj <- decodeJson res
    arr <- obj .? "results"
    traverse (decodeJson <=< flip (.?) "name") arr
```

Now that we have this helper function, we can handle new searches that users perform. First, we'll put our typeahead into the `#!hs Loading` state to represent an ongoing request. Then, we'll empty out the old items in `Select` to avoid out-of-sync data. Then, we'll fetch and decode our items, convert the result from `#!hs Either` to `#!hs RemoteData`, and finally set it on `#!hs State`.

Once our new items have been set, we can use the result to update `Select` just like we did when we handled new selections or removals.

```hs
Select.Searched string -> do
  H.modify _ { items = Loading }
  _ <- H.query unit $ Select.replaceItems []
  newItems <- H.liftAff (fetchItems string)
  H.modify _ { items = fromEither newItems }

  st <- H.get
  _ <- H.query unit
    $ Select.replaceItems
    $ difference (withDefault [] st.items) st.selections
```

That's it! Our typeahead has all the logic necessary to function as required. All that's left to do is actually write the render function.

## Rendering

We have all the state and behavior necessary to run a working typeahead. Now, let's write the render function.

When you write a render function for a `Select` component, keep in mind that the function is going to be run *by* the child component. You can see this right away from the type signature of the render function that `Select` expects:

```hs
myRenderFunction :: Select.State item -> Select.ComponentHTML o item
```

In our case, we've already specialized our parent query, item, and effects, so the type signature is actually this:

```hs
myRenderFunction
  :: Select.State String
  -> Select.ComponentHTML Query String
```

!!! tip
    When you write a component with `Select`, you'll usually want access to the parent component's state and query algebra. This is what makes the pattern powerful: you can use any values from your state in the render function you provide to `Select`, and you can embed any queries from your query algebra, too. Most of the time developers will stay in scope with the parent component by writing the `Select` render function within a `#!hs where` clause. However, you could also write a render function outside the parent component so long as it takes the parent state as an argument, like this:

    ```hs
    selectRenderFunction
      :: Parent.State
      -> Select.State String
      -> Select.ComponentHTML Parent.Query String
    ```

    Then you can write this render function anywhere you'd like while retaining access to the parent's state and query algebra. `Select` will not accept this function as-is, however; you'll need to apply it to the parent state before sending the function in as input:

    ```hs
    selectInput = { ..., render: selectRenderFunction parentState, ... }
    ```

Since you write the render function to pass to Select, you retain full control of the design and most of the structure of your HTML. All that `Select` expects from you is that you apply the three helper functions from `#!hs Setters`:

- `#!hs setItemProps` on each item that can be selected
- `#!hs setContainerProps` on the parent HTML element of all the items
- `#!hs setInputProps` on the text input

For this reason, I usually break my component's render function into three helpers. Let's go ahead and write our render function for the typeahead.

Our overall function is going to take the parent state, the `Select` state, and output the `Select` component HTML type:

```hs
typeahead
  :: State
  -> Select.State String
  -> Select.ComponentHTML Query String
typeahead parentState childState = ...
```

Let's write this function from top to bottom. We want our typeahead to have the list of selected items above the input field, then the input field, then the list of available items (if there are any). Ultimately, with helper functions, we'd like to write this:

```hs
typeahead parentState childState =
  HH.div_
  [ renderSelections, renderInput, renderContainer ]
  where
    ...
```

Let's start with the first one: `#!hs renderSelections`. This function will leverage only the parent state, which contains the selections, and won't use anything from `Select`.

!!! tip
    In fact you don't need to render the selections inside `Select` at all -- you could render the selections first, and then mount the `Select` component below. We use this approach for our own typeaheads at CitizenNet. However, embedding the selections into the `Select` component allows me to show off how embedding parent queries works and it's an equally viable design, so that's the approach taken here.

We're just going to render an unordered list of items that have been selected. If the user clicks on one of them, then we'll remove the selection. We can remove items with the `#!hs Remove` query that we wrote a little earlier.

But wait! Since this is rendering inside of `Select`, it needs to have the `Select` type signature. If we try to write this function it will fail:

```hs
-- The items that have already been selected and can be removed
renderSelections :: Select.ComponentHTML Query String
renderSelections =
  HH.ul_
  ( st.selections <#>
      (\item ->
        HH.li
        [ HE.onClick $ HE.input_ $ Remove item ]
        [ HH.text item ]
      )
  )
```

The compiler gives us this error:

```hs
Error found in module Component

  Could not match type

    Free (QueryF t2 t3 t4)

  with type

    Query

while trying to match type Free (QueryF t2 t3 t4) Unit
  with type Query Unit
```

The problem is that we're using a parent query in the body of a render function for a component with an entirely different query algebra. If we tried to run this in `Select` it would have no idea what to do with the `!#hs Remove` query! Instead, we need to **embed** this query.

To embed a parent query into `Select`, we'll use a query from `Select` called `#!hs Raise` and a message called `#!hs Emit`.

We've already seen `#!hs Emit` before -- when we receive this message, we simply evaluate the query within it. That's how we can evaluate queries like `#!hs Remove` in the parent component even though the event actually happened inside its child, the `Select` component.

`#!hs Raise` is a new one: this query exists to wrap parent queries so they can be embedded. That's why `Select` carries around your parent query in its type signature everywhere!

As a rule of thumb, any time you need to extend functionality in `Select`, you will:

1. Write the new functionality as a query in your parent component and accompanying `#!hs eval` handler. This handler can freely trigger queries and updates in `Select`, or modify parent state that is then used in the `Select` render function. This is quite powerful!
2. Ensure that you are handling output messages from `Select`, and specifically that when you receive the `#!hs Emit` message that you recursively evaluate it as was demonstrated earlier in the tutorial.
3. Place the query in `Select`'s render function wrapped in `#!hs Select.raise`, triggered by whatever event you would like.

Let's see all of this in action:

```hs
renderSelections =
  HH.ul_
  ( st.selections <#>
      (\item ->
        HH.li
        [ HE.onClick $ Select.always $ Select.raise $ Remove item unit ]
        [ HH.text item ]
      )
  )
```

Now we can use this inside `Select` and it will behave just as if it had been written in the parent all along!

Let's move on to the input field. This field needs to be controlled by `Select` and must have the `#!hs setInputProps` helper used on its array of properties:

```hs
-- The text input field that will capture key events
renderInput = HH.input ( Setters.setInputProps [] )
```

That's it! Now we have all the key events wired up for you. You could embed your own queries here, or add CSS, or whatever you want and the behavior will still work just fine.

!!! warning
    `Select` will append the properties it needs to the input field, including `#!hs onMouseDown`, `#! onValueInput`, and so on. Unfortunately there can only be one of these handlers in the list of properties, so if you already placed an `#!hs onValueInput` handler it will be overwritten by `Select`. If you need to trigger some new functionality from the same handler that `Select` is using, then you can always write a custom `#!hs setInputProps` function for yourself that routes the event to your own query *and* the relevant `Select` query. Take a look at the module documentation for `Select.Setters` to see how.

Next, let's render the actual items. Remember that we need to use `#!hs setContainerProps` on the containing element (in this case `#!hs HH.ul`) and `#!hs setItemProps` on each item.

This code is a little trickier. We only want to show the items when the user has focused the typeahead and hide them otherwise. If there are no items, then we want to embed a "Refresh Data" button with some custom functionality. And we want to use `Select`'s information about which item is highlighted to apply a little CSS.

```hs
-- The parent element holding the items
renderContainer = case childState.visibility of
  Select.Off -> HH.text ""
  Select.On ->
    HH.ul
    ( Setters.setContainerProps [] )
    ( case null childState.items of
        false ->
          mapWithIndex renderItem childState.items
        _ ->
          [ HH.li
            [ HE.onClick
              $ Select.always
              $ Select.raise
              $ H.action
              $ HandleSelect (Select.Searched "")
            ]
            [ HH.text "Fetch data again." ]
          ]
    )

-- Each individual item, which will receive an index and the item
renderItem ix item =
  HH.li
  ( Setters.setItemProps ix
      -- If this is the highlighted item, then apply CSS
    $ case Just ix == childState.highlightedIndex of
        true -> [ HP.attr (HH.AttrName "style") "color: red;" ]
        _ -> []
  )
  [ HH.text item ]
```

To recap, we'll use these helper functions in the overall render function we're passing to `Select`:

```hs
typeahead parentState childState =
  HH.div_
  [ renderSelections, renderInput, renderContainer ]
  where
    renderSelections = ...
    renderInput = ...
    renderContainer = ...
```

If you got a little lost in all the rendering code here, don't worry: the full code is contained at the end of the tutorial.

## Conclusion

That's it! We now have a fully-functioning typeahead that will fetch data remotely after debouncing a user's search, and if there are no results, will allow the user to refresh the data. Notably, several parts of this typeahead are not supported in any way by `Select`, but we've been able to freely extend the component to make this possible.

### Next Steps

Now that you're able to build a typeahead with `Select` you know everything you need to build more complex components like date pickers and image pickers. The next tutorial in the series, *Let's build a date picker*, is currently a work in progress.

!!! tip
    Did you notice anything you would improve about this tutorial or the `Select` library? I'd love to hear about it! Feel free to reach out on the [functional programming Slack](https://functionalprogramming.slack.com/) or on the [PureScript user forum](https://discourse.purescript.org). If you found a bug or would like to make an improvement, please open an issue or pull request on the library.

### Source Code

If you'd like to use this component as a starting point from which to build your own, feel free to copy/paste the source code below.

??? article "Full source code for the tutorial"

    ```hs
    module Component where

    import Prelude

    import Effect.Aff (Aff)
    import Effect.Aff.Class (class MonadAff)
    import Data.Argonaut (Json, decodeJson, (.?))
    import Data.Array (difference, filter, mapWithIndex, null, (:))
    import Data.Either (Either)
    import Data.Maybe (Maybe(..))
    import Data.Time.Duration (Milliseconds(..))
    import Data.Traversable (traverse)
    import Halogen as H
    import Halogen.HTML as HH
    import Halogen.HTML.Events as HE
    import Halogen.HTML.Properties (attr) as HP
    import Network.HTTP.Affjax as AX
    import Network.HTTP.Affjax.Response as Response
    import Network.RemoteData (RemoteData(..), fromEither, withDefault)
    import Select as Select
    import Select.Setters as Setters

    data Query a
      = HandleSelect (Select.Message Query String) a
      | Remove String a

    type State =
      { items :: RemoteData String (Array String)
      , selections :: Array String
      }

    type Input = Unit

    type Message = Void

    type ChildSlot = Unit
    type ChildQuery = Select.Query Query String

    component :: ∀ m
      . MonadAff m
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
        { items: NotAsked
        , selections: []
        }

      fetchItems :: String -> Aff (Either String (Array String))
      fetchItems str = do
         res <- _.response <$>
           ( AX.get
           $ Response.json
           $ "https://swapi.co/api/people/?search=" <> str
           )


         pure $ do
           obj <- decodeJson res
           arr <- obj .? "results"
           traverse (decodeJson <=< flip (.?) "name") arr

      render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
      render st =
        HH.div_
        [ HH.slot unit Select.component selectInput (HE.input HandleSelect) ]

        where

        selectInput :: Select.Input Query String
        selectInput =
          { inputType: Select.TextInput
          , items: []
          , initialSearch: Nothing
          , debounceTime: Just $ Milliseconds 250.0
          , render: typeahead st
          }

        typeahead
          :: State
          -> Select.State String
          -> Select.ComponentHTML Query String
        typeahead parentState childState =
          HH.div_
          [ renderSelections, renderInput, renderContainer ]
          where
            -- The items that have already been selected and can be removed
            renderSelections =
              HH.ul_
              ( st.selections <#>
                  (\item ->
                    HH.li
                    [ HE.onClick $ Select.always $ Select.raise $ Remove item unit ]
                    [ HH.text item ]
                  )
              )

            -- The text input field that will capture key events
            renderInput = HH.input ( Setters.setInputProps [] )

            -- The parent element holding the items  container
            renderContainer = case childState.visibility of
              Select.Off -> HH.text ""
              Select.On ->
                HH.ul
                ( Setters.setContainerProps [] )
                ( case null childState.items of
                    false ->
                      mapWithIndex renderItem childState.items
                    _ ->
                      [ HH.li
                        [ HE.onClick
                          $ Select.always
                          $ Select.raise
                          $ HandleSelect (Select.Searched "") unit
                        ]
                        [ HH.text "Fetch data again." ]
                      ]
                )

            -- Each individual item, which will receive an index and the item
            renderItem ix item =
              HH.li
              ( Setters.setItemProps ix
                -- If this is the highlighted item, then apply CSS
                $ case Just ix == childState.highlightedIndex of
                    true -> [ HP.attr (HH.AttrName "style") "color: red;" ]
                    _ -> []
              )
              [ HH.text item ]


      eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message m
      eval = case _ of
        Remove item next -> do
          H.modify \st -> st { selections = filter (_ /= item) st.selections }
          st <- H.get
          _ <- H.query unit
            $ Select.replaceItems
            $ difference (withDefault [] st.items) st.selections
          pure next

        HandleSelect message next -> case message of
          Select.Searched string -> do
            H.modify _ { items = Loading }
            _ <- H.query unit $ Select.replaceItems []
            newItems <- H.liftAff (fetchItems string)
            H.modify _ { items = fromEither newItems }

            st <- H.get
            _ <- H.query unit
              $ Select.replaceItems
              $ difference (withDefault [] st.items) st.selections

            pure next

          Select.Selected item -> do
            H.modify \st -> st { selections = item : st.selections }
            st <- H.get
            _ <- H.query unit
              $ Select.replaceItems
              $ difference (withDefault [] st.items) st.selections
            pure next

          Select.VisibilityChanged vis ->
            pure next

          Select.Emit query -> do
            eval query
            pure next
    ```
