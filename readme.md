# Select [![CircleCI](https://circleci.com/gh/citizennet/purescript-halogen-select.svg?style=badge)](https://circleci.com/gh/citizennet/purescript-halogen-select)

`Select` provides flexible building blocks for selection interfaces in Halogen. If you need a dropdown menu, typeahead, autocomplete, multi-select, calendar, image picker, or other selection interface, and you want it to be accessible, and you also want complete visual control over the component, then you're in the right place.

- [Module Documentation on Pursuit](https://pursuit.purescript.org/packages/purescript-halogen-select)
- [Official Documentation / Tutorials / Getting Started](https://citizennet.github.io/purescript-halogen-select)
- [CitizenNet UI Repository](https://citizennet.github.io/purescript-ocelot)

**This library is under active development and changes often. If you need a stable selection library, we're not there yet! Please consider helping out with a pull request to add features you need and help us reach 1.0!**

## Get Started / Learn More

There are a few ways to get started with the `Select` library.

**Installation**  
`Select` is available on Bower and Pursuit:

```sh
bower i --save purescript-halogen-select
```

**Pursuit Package Documentation**  
[The Select library and its module documentation are on Pursuit](https://pursuit.purescript.org/packages/purescript-halogen-select).

**Official Docs, Tutorials & Getting Started**  
To learn more about using `Select`, see [the official documentation site](https://citizennet.github.io/purescript-halogen-select).

**Live Code**  
You can see working examples of components built using this library in a few places:

- See the [Components](https://github.com/citizennet/purescript-halogen-select/tree/master/docs/src/Components) folder from the documentation site, or
- Check out the official [CitizenNet UI Repository](https://citizennet.github.io/purescript-ocelot) where we use this library to make our app's design system.

Have an example of a component you've built with `Select`? Open a PR or drop us a message and we can help review and showcase your work.


# Design Principles  

The library provides essential behaviors for selection user interfaces as a group of Halogen components. But you won't find a single render function in the code. Instead, with the help of a few `getProps` helpers, you can write your HTML rendering however you'd like. You can freely include your own queries and the library will return them to be run. You can even use any data you want from your parent state in your render functions. The library manages user interaction, state, accessibility, and logic; you are responsible for rendering HTML depending on that state.

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

For example, you can make your container compatible with the component with the `getContainerProps` function, which you would apply to the array of iprops present on your button element:

```purescript
[ ... your HTML ...
, HH.span
    ( setContainerProps [ ... your css & events ... ] ) -- Augments your props with our behaviors
    [ ... your HTML ... ]
, ... your HTML ...
]
```

> Warning: If your events are duplicated by ours, they will be overwritten and fail to trigger.


## Inspiration & Thanks

This project drew inspiration from the approach taken by [paypal/downshift](https://github.com/paypal/downshift). Special thanks to [Nathan Faubion](https://github.com/natefaubion) and [Nicholas Scheel](https://github.com/MonoidMusician) for their help.
