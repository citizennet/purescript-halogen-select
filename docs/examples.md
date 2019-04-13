title: PureScript Halogen Select Examples

# Examples

You can play around with a few example components here. However, for a much richer set of components with much more functionality, check out the [Ocelot design system by CitizenNet](https://citizennet.github.io/purescript-ocelot/#typeaheads).

!!! warning
    The components on this page function properly, but look horrible while we migrate CSS.

### Dropdown

Dropdowns are a common button-driven input type, especially for navigation. But most custom dropdowns sacrifice usability: unlike browser default dropdowns, you can't type on most custom dropdowns, nor are many built with accessibility in mind. With `Select` you can easily create rich, usable dropdowns with little code.

<div data-component="dropdown"></div>

Curious how to build a dropdown with `Select`? Check out [the dropdown tutorial](https://citizennet.github.io/tutorials/dropdown).

### Typeahead / Autocomplete

This library was originally designed so that we could build typeaheads with all sorts of custom rendering and functionality. It was frustrating to find solutions that almost worked, but broke down as soon as you needed a moderate level of customization.

Building typeaheads with `Select` is only a little more complex than building dropdowns. Instead of a button as input, you'll use a text input, and you'll be responsible for deciding how to handle user searches. `Select` handles debouncing user input, keyboard navigation, and more on your behalf.

The typeahead below is quite simple; to see examples of more sophisticated typeaheads -- including ones that fetch and display data asynchronously -- check out the [Ocelot component library](https://citizennet.github.io/purescript-ocelot/#typeaheads).

<div data-component="typeahead"></div>

Curious how to build a typeahead with `Select`? Check out [the typeahead tutorial](https://citizennet.github.io/tutorials/typeahead).
