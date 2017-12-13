# Select

This project provides the core building blocks for common select fields, including:

- Dropdown menus
- Typeaheads & autocompletion
- Multi-selects

It also exports these building blocks composed together into working components you can drop in to your Halogen project, or use as a template you modify. See the [examples]() folder for working examples of these in action.

## Design Principles

1. Provide behaviors, not styles  
Developers should be able to style and display dropdowns and typeaheads however they would like, rather than be forced to use particular CSS classes or re-implement the component with their HTML. This is accomplished with augmented render functions as described below. We provide the machinery; you provide the HTML and styles.

2. Export the building blocks, not just the end result  
Developers should be able to take a core set of behaviors and choose how they would like to handle them in their own version of the component. If you would like the typeahead's functionality but do something fancy with the selected items, you should be able to. Each building block is exported.

3. Require minimal configuration  
Instantiating a typeahead shouldn't require a 50-field configuration record. We require at minimum two things: the data to populate the menu and the HTML to render that data. The rest is taken care of by the component. You are responsible for handling two things: when an item was selected, and when the user has performed a new search. If you want to do even less, you can use one of our default implementations to drop in to your project.

4. Be accessible (Upcoming)  
ARIA props and other features necessary for accessibility online should be handled properly without any setup.


# Rendering
The primary design decision made in this project vs. other typeaheads is offloading HTML rendering to the user. Rather than render an input field ourselves and provide a CSS class for styling, we allow you to write your own HTML and augment your properties with the behaviors we need for the select to function.

In practice, you are responsible for providing render functions for each HTML element involved in the component. Your render function should be augmented using one of our provided functions, which will extend your CSS & events with our behaviors.

For example, you can make your menu toggle compatible with the component with the `getToggleProps` function, which you would apply to the array of iprops present on your button element:

```purescript
[ ... your HTML ...
, HH.button
    (getToggleProps [ ... your css & events ... ]) -- Augments your properties with our behaviors
    [ ... your HTML ... ]
, ... your HTML ...
]
```

This carries one important restriction: **the render function must carry the typeahead or dropdown's query type, not the parent's.** If you want to include any event handlers of your own (for example, you want a modal to open when the user focuses the input field), then you will need to wrap it in the `ParentQuery` constructor from the typeahead or dropdown. In practice:

```purescript
-- Wrap your queries using embedParentQuery
HH.input $
  getInputProps [ HE.onBlur $ HE.input_ $ embedParentQuery YOURQUERY ]

-- Under the hood, embedParentQuery is doing this:
HH.input $
  getInputProps [ HE.onBlur $ HE.input_ $ Dropdown.ParentQuery (H.action YOURQUERY) ]

-- Contrast with this, which won't work because the HTML has your query type, not
-- the typeahead or dropdown's:
HH.input $
  getInputProps [ HE.onBlur $ HE.input_ YOURQUERY ] -- type error
```

> Warning: As of 12/12/2017, if your events are duplicated by ours, they will be overwritten and fail to trigger. Support for multiple handlers from the same event is not yet supported.


# Major Parts
This project is made up of several building blocks. Each part includes:

- Some data that will populate the menu, along with a render function that describes how the data should display
- Some number of HTML elements necessary to register behaviors (like an input field the user can type in)
- For each element: a render function that describes how it should display
- Some number of events that need to be communicated to the parent so it can take action (like notifying you that a particular item has been selected)

In each section we describe the render functions and output messages involved in the layer. Each layer will require the functions and messages noted in the previous layer.

## Dropdown
The bottom layer is the minimum functionality necessary for a dropdown menu: some clickable region to toggle the menu on and off, and some list of options that can be selected. Notably, the parent is responsible for managing the list of selected items; the menu simply displays the available options.

### Render Functions
**`toggleHTML`**  
The HTML for the clickable region that will open and close the dropdown.

**`itemHTML`**  
The HTML for each item listed in the dropdown

### Messages
**`Selected item`**  
One of your items has been selected. You are expected to handle this, and if you need to modify the data provided to the menu, send in new data using the menu's query type.


## Typeahead
The typeahead extends the functionality of the options menu with the ability to type and search the list of available options, which can be debounced if you need to make asynchronous requests to select that data.

### Render Functions
**`inputHTML`**  
The HTML for the input field the user is able to search with.

### Messages
**`Search item`**  
The user has performed a new search. This is equivalent to an `onChange` event. The typeahead can handle debouncing on your behalf if you would like.


## Select
The select extends the functionality of the dropdown OR the typeahead (the typeahead necessarily includes the dropdown** by managing the list of selected items.

### Render Functions
**`selectedHTML`**  
The HTML for items that have been selected by the user.

### Messages
**`Remove item`**  
The user has clicked to remove an item that was selected


# Inspiration & Thanks

This project drew inspiration from the approach taken by [paypal/downshift](https://github.com/paypal/downshift). Special thanks to [Nathan Faubion](https://github.com/natefaubion) and [Nicholas Scheel](https://github.com/MonoidMusician) for their help navigating type issues in Halogen.
