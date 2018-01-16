# Select [![CircleCI](https://circleci.com/gh/citizennet/purescript-halogen-select.svg?style=badge)](https://circleci.com/gh/citizennet/purescript-halogen-select)

This project provides the core building blocks for common select fields, including:

- Dropdown menus
- Typeaheads & autocompletion
- Multi-selects

It also exports these building blocks composed together into working components you can drop in to your Halogen project, or use as a template you modify. See the [examples](https://github.com/citizennet/purescript-halogen-select/tree/master/examples) folder for working examples of these in action.

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
    ( getToggleProps [ ... your css & events ... ] ) -- Augments your properties with our behaviors
    [ ... your HTML ... ]
, ... your HTML ...
]
```

> Warning: As of 1/12/2017, if your events are duplicated by ours, they will be overwritten and fail to trigger. Support for multiple handlers from the same event is not yet supported.


# Inspiration & Thanks

This project drew inspiration from the approach taken by [paypal/downshift](https://github.com/paypal/downshift). Special thanks to [Nathan Faubion](https://github.com/natefaubion) and [Nicholas Scheel](https://github.com/MonoidMusician) for their help navigating type issues in Halogen.
