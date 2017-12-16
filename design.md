# Design Philosophy

We have chosen to design this library using primitive components. These components exist to provide the machinery for selecting things while leaving as much of the visual aspect to the user as possible. The idea is that the user can build for themselves any kind of selection component by combining our primitives together. The display of the component would be completely under their control, but its behaviors would be pre-written by us. In other words: a maximum of visual control with a minimum of effort devoted to functionality.

Another way to put it: we are providing a few common elements that go into selecting things in user interfaces. Usually, you’ll have a list or matrix of items; these items can be clicked on, or navigated with arrow keys, or selected with the “Enter” key; they might be searchable using some input field; the data might be all available from the start, or it might continually refresh; selected items might be selectable again (like a shopping cart) or not (like a country selector); and so on.

These elements can all be described with some set of possible states and the behaviors that transition the element among those states. Of course, these elements are related: an action taken on one element might transition its state, and also notify other elements that it has changed so they can transition their own states (if necessary).  For example, an “input” element might have various text-based states and transition among them when the user clicks or types; however, it might also communicate with other elements when the user presses a key. A down arrow pressed on the input field might serve to trigger a behavior in a menu list element elsewhere. Notably, the input field never directly causes a behavior outside itself. It simply notifies the world at large that it has been changed.


## On primitives
This is what our primitives are. Each primitive is one element common to selection functionality in user interfaces and must contain:
- some set of possible states
- some set of behaviors that transition among states
- (optionally) some set of notifications that something important has happened

Our idea of a “primitive” notably eliminates some functionality:
- they do not contain children, and so the behaviors of other elements aren’t handled (the primitive only worries about itself and its possible states).
- they do not decide how they will display, and they do not control what will trigger their behaviors. They simply provide the mechanism to transition, while the cause must come from outside.
- they do not share behaviors with other elements; they do not “wrap” any shared functionality, including toggles or things of that nature.

If we commit to this model, then we agree to a specific design decision:
- Primitives are HTML elements that group actions together & are common to how people select things. That means our primitives will be things like “input”, “toggle”, and “item.” Notably this is not “menu” or “typeahead.” You must pull in the elements you think are relevant and you must wire them up.


### Other helpful notes
Conceptually, you can think of a “primitive” as a “group of behaviors and messages.” Since these are generally triggered by the user, and the user is in the browser, this means a primitive is usually tied in to a particular HTML element that supports the events we need to cause those behaviors. For our purposes, this can be _any_ HTML element for most primitives, and the `input` element for primitives that use text.

It is important to distinguish between behaviors and events. For example, the `Item` type contains no behaviors. It doesn’t do anything. But it can be clicked — that’s an event. Thus the `Item` type cannot be a primitive (no behaviors). On the other hand, a container usually has no events at all, but it does have state transitions (change the highlighted element, open, close). Since we aren’t writing the renderer, we really don’t care about events. We care about states and transitions. Thus `Container` _is_ a primitive.

I’ve described a catalog of common selection user interfaces and all the primitives that would be necessary to construct them in the _Select Catalog_ and _Primitive Catalog_.


## How can primitives be combined?
Primitives must exist as independent sibling components managed by a single parent. For a full explanation of why this is necessary and cannot be bent even a little bit, see the section titled **Why can’t primitives have children?**. This necessitates a parent component that will require the end user to do their own configuration. However, we can also provide sample designed components on their behalf.

When the library is used, the design workflow will look like this:

1. Create a new Halogen component that will contain the primitives, including your own queries, render function, and eval function.
2. Decide what primitives are necessary to combine together to make your component.
3. For each primitive:
	1. Add a new slot type to your ChildPath.
	2. Write a render function that triggers the primitive’s state transitions (or use our helper for it to be automatically filled out for you). Embed any of your own queries if you want.
	3. Fill out the primitive’s input type, giving it its initial state.
	4. Decide where to mount the slot in your HTML.
	5. For each emitted message, decide if you want to handle it. If so, write the handler. Usually this will simply involve routing the message to another primitive. If not, you can ignore the message.
4. If you need additional functionality not provided by primitives (for example, you want to incorporate paging functionality to move through the results), add it yourself, and use it to trigger state transitions in the primitives as you see fit.

Our use of primitives means that the end user can build up a menu out of them, and yet retain the flexibility to extend their menu with whatever additional functionality they want. The fourth step in the design workflow is probably the most important one — the user can freely augment and extend the behaviors we have provided!

It also means the user gets stuck writing handlers all the time. You have to handle potentially a dozen or more messages from primitives, much of which comes down to routing them over to other primitives. Not much machinery is hidden from you. On the other hand, we can certainly write helper functions to help ease the pain of hooking up all this machinery.


## Why can’t primitives have children?
Halogen has two restrictions that point to just one viable way to design a system of primitives. If we do not design primitives as atomic siblings, we rapidly lose the point of this design method at all and would be better off making a family of selection components that maintain their own internal behaviors (and quite a lot of duplication).

The short version: independent sibling elements orchestrated by a single parent are the only way to “build something out of parts.” Primitives cannot have children.

1. Query types are algebraic data types. Like all ADTs, you can’t simply “mash them together.” If you have been given two ADTs and want to use them both, you have to wrap them in a further type — like `Either Type1 Type2`.  In Halogen, HTML involves a query type; if you want a block of HTML to trigger events for more than one query type (for instance, for two different types of components), then you have to wrap the entire block in a higher query type. Unlike wrapping something in `Either`, wrapping something in a higher `Query` type involves making a whole wrapper _component_, which is a high overhead.

2. When components are mounted, they’re given an address. This address is called a “slot”. If a parent component wants to trigger an action in a child component, it has to provide the action & the child’s address to send it to. Children have no awareness of the addresses of other components. They can only communicate with their parent. This means that any group of primitives must be orchestrated by some parent component.

These two points mean that a design in which one primitive extends the behavior of another one carries unacceptable overhead. As an example, consider a typeahead in which an input field wraps and controls a menu list. You need to:

- Wrap item queries within input queries within the parent query, which is tedious;
- Maintain the item slot type within the input type so it can be accessed to trigger sub-queries (none of the nested primitives would define the slot types because none of the nested primitives define the render functions that use them!);
- Add queries to the algebra to actually trigger those sub-queries;
- Pass messages up and down multiple levels in a hierarchy;
- Pass render functions down multiple levels of a hierarchy, none of which can be maintained in State types for the involved component and have to be passed as arguments
- Maintain state across multiple levels in a hierarchy

It also becomes impossible to simply “add on” another primitive. You must perform this process once again at yet another level — ADTs cannot simply be merged together, and in Halogen, a new query ADT means a new component!


## Design Limitations
When designing with primitives, the user will find themselves in an odd situation — a bunch of parts, all of which are emitting various signals, and which, when given a particular input, will change states, but no cohesion. It’s like you’ve been given a collection of sensors without a central brain, and you’ve got to figure out how you should stick them all together.

This is no small effort. The more small primitives you use, the more messages and queries you have to handle and shuffle around. The more render functions you have to write. Not much is encapsulated. The effort of wiring everything up may not offset the benefit of not having to write your own behaviors enough for the library to be useful.

Some primitives may be _too_ small, even if they are important parts of a selection UI. Should we really provide a “clear all” button component that exists to emit one message? And therefore force the parent to add it to their slot type? Or should they handle that themselves? What about paging components, like a “next” button?


### Ways to make this more usable
There are not many ways in which we can minimize the effort involved. Below are the two I think cover enough cases to be viable:

1. Generous helper functions for common functionality, like functions to attach all our triggers to an HTML element;

2. Examples of many common menu types built out; we may be able to export some of these, but at worst, the user can copy and paste them into their own projects;

3. Pre-built menu types; we could build some components, like a menu, out of parts so things like passing messages between a toggle and a container of selectable items is easy. The user is still responsible for render functions and items, but we’ve already handled the internal state stuff for them. These would be much less flexible and extendable, but a far easier setup time for the end user.

4. Documentation, documentation, documentation.



# Select Catalog
There are several common user interfaces for selecting different kinds of things. There are menus/dropdowns, typeaheads, date pickers, image pickers, color pickers, and so on. Not every selection UI will be possible with this library, but most common use cases are eligible.

We may be able to offer these as pre-built components, in which the user just provides partial render functions and data.

## Menu
  Parent
  |——— toggle button
  |——— list container of option items
  |——— list container of selection items

The toggle button is responsible for registering various events and emitting messages when important ones have happened. For example, it will register things like “The user wants to select the next element” or “The user has toggled the menu.” But it will not _do_ any of these things itself. It is so simple its only states consist of what click or key press is occurring on it at any moment and it raises these as messages. That is its purpose.

The container of options does something different. It is responsible for maintaining which item contains a user’s focus, or which item has been clicked on (selected), or translating input messages like “The user wants the next one” into a decision about how you choose “the next one.” It emits messages, too, just like the button does. For example, it might say “This item has been selected.”

The container of selections does something still different. It describes things that have been chosen and can be removed. It tells the parent when the user has said “I want to remove this one.” It might handle dragging and re-ordering. It might even have its own dedicated button sibling the parent can instruct to toggle and register events on the selection container.

	- Why a “container”? Why isn’t the individual item the primitive? The reason the container is the primitive is because of the encapsulation of _behavior_. Individual options don’t have behaviors. They can raise a message that they’ve been clicked, perhaps, but that’s it. It’s the container that has behaviors: some list that maintains which index is focused, which element has been clicked, and so on. However, there can be different sorts of containers dependent on what sort of behaviors they support: a selection container might handle clicks on items and interpret them as “this item should be removed.”, or allow features like drag & drop.

The menu parent is responsible for orchestrating the three. It provides the render function for the button and for the container of items. It listens to messages from the button and the container and decides what to do about them. It might render other things altogether; if there are no items, it might not render the container at all, and may instead render something else (like an error message). It can trigger transitions in state for its children, like telling the container to close.


## Typeahead
Parent
|——— toggle button
|——— input
|——— list container of option items
|——— list container of selection items
|——— clear button

The toggle button, container of options, and container of selections are the same as before and behave the same. In this instance, the toggle button might be a down arrow next to the input that toggles the menu. Of course, if the parent doesn’t want all the functionality of the focus — for example, it doesn’t care about “enter” events from the toggle because the input field is there — it can simply ignore the messages the toggle button will faithfully emit.

The input is new. It captures user input, optionally debounces it, and tells the parent when a new search is ready. The parent has to decide what to do with that search, of course. It might also tell the parent about the same focus events that the toggle button did. (Functionality is duplicated because of this. Primitives must wrap all their behaviors in themselves. But we can probably abstract some of this into a shared utility module).

The clear button simply raises a message indicating the user’s desire to clear all selections.

To create a multi select, the parent simply does not tell the menu to close when an item has been selected, or implements its own logic for this (for example, force a close only if 3 items have been selected). To create an “insertable” element, in which you can add an item if it isn’t already in the list, the parent again is responsible. When receiving a search, it can insert the that search into the list of items sent back to the list of options with a label indicating it can be inserted, and also handle the insertion.


## Searchable Image Selector
Parent
|——— input field
|——— grid container of selectable items
|——— prev pager
|——— next pager

The input field captures searches; the parent may then load data asynchronously (for instance, by querying Shutterstock). This is all the same as before and requires no modification.

The container is new. It has two differences from before.

First, it is a _grid_ instead of a _list_. This is important because it means we support matrix operations — you can use the left and right arrows, for example, to navigate through the grid. This requires a different set of navigation behaviors when keys are pressed on the container, as well as a different way to handle highlighting.

Second, it contains _selectable options_ instead of just ‘options’. In other words, we’re merging the behaviors of selections and options together. On a click event: if the item is “selected” then it will be “removed”; if it is “unselected” it will be “selected.” For a single select the parent can elect to only allow one at a time; for a multi select, the parent could allow as many as it wishes.

	- Next, we have the pagers. Here, I made a conceptual mistake: I thought of them as a pair of clickable elements determining if the user wants to go forward or back. But this is wrong! **All primitives have one HTML element.** So if you want multiple of these, what you’re really talking about is a prev pager and a next pager, or perhaps a “pager” that can be prev OR next, but never two elements.

Next are the ‘prev’ and ‘next’ pagers. Each is a clickable element indicating the user wants to navigate in one direction or another. They are aware of the limits of the supplied elements, so they can decide if they should be disabled or not.

As always: the various elements exist to register events and send meaningful messages, but the parent is there to orchestrate the primitives.


## Input + Calendar Date Picker
Parent
|——— prev pager
|——— next pager
|——— input field
|——— grid container of selectable items

The same elements show up again, and the only difference here comes down to the items provided. The user can select a particular item from the list of dates; the parent can disable items that are outside of the intended range (only enabling the visible month’s valid dates); the user can navigate with arrow keys and select with “enter”; when a debounced search has been performed in the input field, the parent can figure out how they would like that to affect the items displayed in the container.
