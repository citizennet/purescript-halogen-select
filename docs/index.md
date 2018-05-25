title: Documentation for PureScript Halogen Select

# Welcome

`Select` helps you build selection user interfaces in PureScript with Halogen. You can use it to build dropdowns, typeaheads and autocompletes, date pickers, image pickers, and more, with features like keyboard navigation, accessibility, and state management handled for you. This library takes a unique approach to component design to ensure you can leverage its features without compromising your design in any way.

## Installation

You can use `Select` in your PureScript project with a compatible package manager. The PureScript community typically leverages [psc-package](https://github.com/purescript/psc-package) or Bower:

```sh
# Using psc-package
$ psc-package install halogen-select

# Using Bower
$ bower install --save purescript-halogen-select
```

## Quick Start

If this is your first time using `Select`, start with the [tutorials](https://citizennet.github.io/purescript-halogen-select/tutorials/getting-started). I'd recommend starting with the simplest example where you'll learn to make a keyboard-navigable dropdown component:

!!! tip
    Don't want to build your own UI components? Check out the [Ocelot component library](https://citizennet.github.io/purescript-ocelot)!

If this isn't your first time, you'll find value in these resources:

* The [how-to](https://citizennet.github.io/purescript-halogen-select/how-to/embed-parent-queries) section contains plenty of short guides for common tasks you'll perform using `Select`. Consider it a grab-bag of useful strategies and examples you can refer to when developing new components.
* The [concepts](https://citizennet.github.io/purescript-halogen-select/concepts/understanding-free-queries) section contains more detailed explanations on the design of the library. It will help you understand how to make good design choices and make the most of the tools available to you.
* The [reference documentation on Pursuit](https://pursuit.purescript.org/packages/purescript-halogen-select) contains the module documentation and source code. It's a useful reference to verify what functions are available to you.
* The [examples folder on Github](https://github.com/citizennet/purescript-halogen-select) contains the working source code for all the components in the [tutorials](https://citizennet.github.io/purescript-halogen-select/tutorials/getting-started). If you're building a similar component, this code can help you get started.

## Why Select?

`Select` provides essential behaviors for selection UI as a flexible, extensible Halogen component. But you won't find a single render function in the code. Instead, with a few helper functions, you can write your own `#!hs State -> HTML` function however you'd like. You can:

* Extend the component's functionality by embedding new queries in the HTML
* Extend the component's data by including as much additional state from the parent as you want (which you can then use in your render function)
* Leverage the provided features for user interaction, state management, accessibility, and logic
* Retain complete freedom over the design and aesthetic of your selection component

!!! aside "For visual learners"
    I gave a talk at the Los Angeles PureScript meetup in April 2018 about the approach this library takes. It provides an overview of our design approach, including advantages and drawbacks, as well as a simple walkthrough of building a dropdown. No, the man in the preview isn't me -- that's [Phil Freeman](http://functorial.com/), the designer of the PureScript programming language.

    <iframe width="560" height="315" src="https://www.youtube.com/embed/igWrktC0m7E?rel=0&amp;showinfo=0&amp;start=2119" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>
