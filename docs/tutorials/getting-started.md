# Introduction
Halogen is a powerful framework for building PureScript applications. It’s used by several companies, including SlamData and my own company, CitizenNet (a Condé Nast company), among others. The `Select` library is written for the Halogen framework, so if you don’t know how to use Halogen yet, you ought to start with the [Halogen guide](https://github.com/slamdata/purescript-halogen/tree/master/docs). That said, with only passing familiarity with Halogen, you should be able to follow along just fine!

## Setup
Instead of creating a new Halogen project from scratch, we’ll start with a minimal starter template. This template includes the HTML, build scripts, and basic `Main.purs` file necessary to run your Halogen application. It also includes a component with the bare minimum definitions in place. This component does nothing at all, which is nice because we can easily use it to start building dropdowns, typeaheads, and other components.

!!! note ""
    We prefer Yarn over NPM for package management and scripts, but either one will work. Anywhere you see `#!sh yarn <script>`, you can substitute `#!sh npm run <script>` instead. Feel free to look at the `package.json` file if you want to see what these scripts are doing.

### Installation

First, clone the Halogen template project from CitizenNet, install dependencies, and make sure things build properly. If they don’t, please reach out on the [Purescript user forum](https://purescript-users.ml) so we can fix it!

Next, make sure to install `Select`:

```shell
bower i --save purescript-halogen-select
```

And that's it! You now have everything you need to complete the tutorials. This is the full set of steps you can follow to get all set up:

```shell
# Get the CitizenNet starter Halogen project
git clone git@github.com:citizennet/purescript-halogen-template.git

# Change into the directory and install packages
cd purescript-halogen-template && yarn

# Install a new package: purescript-halogen-select
bower i --save purescript-halogen-select

# Build the project
yarn build

# Open the application in the browser
open dist/index.html
```

After you complete each step in the tutorial, make sure to rebuild the project and refresh your browser to see your updated component.

### Helpful tip: Watching for file changes
It’s convenient to keep a terminal running which watches for file changes, rebuilds the project, and bundles JavaScript on your behalf. Then, when you make a change to a file, all you have to do is wait a moment and refresh the page to see your updates.

When I write PureScript, I usually work with two terminals open. I use the first to write code, and the second to watch those changes and rebuild. I recommend using the same technique as you walk through these tutorials. These three steps are all you need:

1. Open a new terminal and run the `#!sh watch` script
2. Open your editor to a source file
3. Open a new tab in your browser pointed to `dist/index.html` so you can see the app

To test everything is working, try editing `src/Component.purs` to change the title of the page. The project should automatically rebuild on save. Then, when you refresh the browser, you should see your new text rendered.

```shell
# Watch for changes and rebuild (remember to refresh the page after builds)
yarn watch
```

## A whirlwind tour of our starter component

The project starts off with a minimal Halogen component. As a brief refresher, I'll step through each of the types and functions involved.

!!! info
    If you are already quite familiar with Halogen, feel free to skip this section entirely.

### Query Algebra

How does a component know what to do?

In Halogen, we give names to each computation we'd like a component to run. Computations that can have side effects but don't return anything are colloquially called *actions*; those that can have side effects and also return something are called *requests*. The type that lists out the possible actions and requests for a component is called the component's *query algebra*. The Halogen guide has a relevant [section about query algebras](https://github.com/slamdata/purescript-halogen/blob/master/docs/2%20-%20Defining%20a%20component.md#query-algebra) if you'd like to know more.

What actions and requests can our starter component perform? By looking at the query algebra, we see just one constructor:

```hs
data Query a
  = NoOp a
```

All we know so far is that this component can do one thing: evaluate a query called `NoOp`. We'll see what it does later on when we look at the `#!hs eval` function.


### State

Every component encapsulates some state, described by its `#!hs State` type. You will usually see Halogen components use records to hold state, like this:

```hs
type State = { on :: Boolean, name :: String }
```

State is the core of your component. Most of the queries you see in Halogen components modify state in some way, and the render function that produces HTML for the component has only the `#!hs State` type as its argument.

For our starter component, we don't need any state just yet, so we've simply assigned it the `#!hs Unit` type. When we start building selection components, however, we'll soon create a record to hold our state.

```hs
type State = Unit
```

### Input

A component's `#!hs Input` type can be thought of as a container for any information you'd like to pass to the component. It's most commonly used to provide a component with some initial `#!hs State` values via the `#!hs initialState :: Input -> State` function. However, it's more powerful than that!

Once a Halogen component has been mounted to the DOM, there is only one way to continue sending it new information: its `#!hs Input` type paired with its `#!hs receiver` function. Every time the parent component re-renders, it will send a new `#!hs Input` to the child component.

For more information on the `#!hs Input` type, see the [Parent and Child Components](https://github.com/slamdata/purescript-halogen/blob/master/docs/5%20-%20Parent%20and%20child%20components.md#input-values) section of the Halogen guide.

Our starter component doesn't need any input, so we'll assign it the `#!hs Unit` type. However, once we build a dropdown or typeahead, we'll probably want to receive the list of items that can be selected as input.

```hs
type Input = Unit
```

### Message

How does a component tell its parent when something important has happened? In Halogen, this is accomplished with a `#!hs Message` type. Like the query algebra, this is just a type describing messages that can be raised, containing some information. To actually trigger sending a particular message, you can use the `#!hs raise` function provided by Halogen.

When we start building selection components, we'll use messages to notify parent components when items have been selected or removed. Our starter component doesn't need to raise any messages, however, so we've given it the `#!hs Void` type.

??? note "Why are we using `Void` when we have no messages?"
    Why use `Void` instead of `Unit` for the `Message` type when it has no constructors? This is common practice in Halogen because of how messages are used by parent components. When your component raises a message, it gets handled by the parent using a function like this:

    `#!hs Child.Message -> Maybe (ParentQuery Unit)`

    If you want to ignore all messages from the child, you could write an implementation like this:

    `#!hs Halogen.HTML.Events.input <<< const Nothing`

    However, if the child's message type is `Void`, then you can use the `absurd` function from `Data.Void`:

    `#!hs absurd :: Void -> a`

    This saves you a bit of typing when you mount a child component in a slot and makes it absolutely unambiguous that there are no messages to handle. It also ensures that if you add a message to the child component later on you'll get a compiler error -- this is a good thing!

    Compare mounting a child component that uses `Unit` to represent "no messages" vs. using `Void`:

    ```hs
    -- It's unclear whether you're ignoring all messages or whether there are
    -- simply no messages to handle.
    HH.slot ComponentSlot component unit (Halogen.HTML.Events.input <<< const Nothing)

    -- It's obvious there are no messages, and if that changes (the component adds a
    -- message) you'll get a nice compile-time error.
    HH.slot ComponentSlot component unit absurd
    ```


For more information on messages, see the [Parent and Child Components](https://github.com/slamdata/purescript-halogen/blob/master/docs/5%20-%20Parent%20and%20child%20components.md) section in the Halogen guide.

```hs
type Message = Void
```

### ChildQuery and ChildSlot

Halogen components often have further child components. To maintain type safety when managing multiple child components, Halogen uses a pair of concepts: *child queries* and *child slots*.

- The **ChildQuery** type lists out each unique type of child component your component has. For each type of child component, you'll add its query type here.

- The **ChildSlot** type works like an address book for the various child components. If you only have one child component of any distinct `ChildQuery`, then you can just use `unit`. However, if you have multiple children with the same query type, you need some way to distinguish between them. It's common to use custom types or integers for this.

See the [Multiple Types of Child Component](https://github.com/slamdata/purescript-halogen/blob/master/docs/5%20-%20Parent%20and%20child%20components.md#multiple-types-of-child-component) section of the Halogen guide for more details.

For now, our component has no children. Once we bring in the `Select` component we'll update these types.

```hs
type ChildQuery = Const Void
type ChildSlot = Unit
```

### Component

Ah! We can finally create our component. The actual component definition is simple: we call the `#!hs parentComponent` function from Halogen to assert we're creating a component that can have further child components and provide it with the four functions it needs to operate. More on those in a moment!

```hs
component :: ∀ eff m
  . MonadAff eff m
 => H.Component HH.HTML Query Input Message m
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }

  where
```

Next, lets look at those function definitions, defined in the `#! where` clause:

#### initialState

The `#!hs initialState` function describes how to go from the component's `#!hs Input` type to its `#!hs State` type. In this case, our `#!hs State` type is just `#!hs Unit`, so we'll throw away the input and return `#!hs unit`.

```hs
initialState :: Input -> State
initialState = const unit

-- Could also be written this way:
initialState = id
```

#### render

The `#!hs render` function describes how to go from the component's `#!hs State` type to some HTML, where that HTML can include any of the components listed in the `#!hs ChildQuery` type. You'll use plenty of code from these modules when writing render functions:

```hs
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
```

We're going to spend a lot of time writing render functions in the following tutorials. You can refer to the Halogen guide's [section on rendering](https://github.com/slamdata/purescript-halogen/tree/master/docs) for more information.

For now we won't render anything to the page, represented by an empty div.

```hs
render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
render st = HH.div_ []
```

#### eval

The `#!hs eval` function describes what to do when one of the queries from the component's query algebra is called. There are various ways a query can be triggered:

- The parent component can trigger a child component query using the `#!hs query` function
- A user can trigger a query with an event in HTML, like `#!hs onClick`
- The `#!hs eval` function can recursively call itself while evaluating a query

The `#! eval` function is where you get to actually define what all your queries *do*. Unlike the render function, you can actually perform all kinds of side effects here, like make API calls, update state, trigger queries in child components, raise messages, and more.

As usual, our starter component won't do much in its `#!hs eval`. When it receives the `#!hs NoOp` constructor, it will do nothing and return the next query.

```hs
eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message m
eval = case _ of
  NoOp next -> pure next
```

#### receiver

The `#!hs receiver` function describes what to do when a parent component sends in new `#!hs Input`. Its type signature looks like this:

```hs
receiver :: Input -> Maybe (Query Unit)
```

Once a Halogen component has been mounted, the only way to send it new input is via its `#!hs receiver` function. When its parent re-renders, it will automatically send the child component's input type again, and it's up to the `#!hs receiver` function to decide what to do with it.

This function can either provide a query to call, or `#!hs Nothing` if you'd like to ignore new input. If you elect to provide a query then you unlock all the power available in the `eval` function and can describe all sorts of things to do on new input, like making API calls or updating state.

In our case, we don't care about new input, so we'll ignore the input and return `#!hs Nothing`.

```hs
{ ...
, receiver: const Nothing
}
```
