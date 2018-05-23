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

If this is your first time using `Select`, start with the [tutorials](https://citizennet.github.io/purescript-halogen-select/tutorials). I'd recommend starting with the simplest example where you'll learn to make this dropdown component:

<div class="ocelot-scoped" data-component="dropdown"></div>

If this isn't your first time, you'll find value in these resources:

* The [user guide](https://citizennet.github.io/purescript-halogen-select/user-guide) contains more detailed explanations on the design of the library. It will help you understand how to make good design choices and make the most of the tools available to you.
* The [how-to](https://citizennet.github.io/purescript-halogen-select/how-to) section contains plenty of short guides for common tasks you'll perform using `Select`. Consider it a grab-bag of useful strategies and examples you can refer to when developing new components.
* The [reference documentation on Pursuit](https://pursuit.purescript.org/packages/purescript-halogen-select) contains the module documentation and source code. It's a useful reference to verify what functions are available to you.
* The [examples folder on Github](https://github.com/citizennet/purescript-halogen-select) contains the working source code for all the components in the [tutorials](https://citizennet.github.io/purescript-halogen-select/tutorials). If you're building a similar component, this code can help you get started.

## Why Select?

`Select` provides essential behaviors for selection UI as a flexible, extensible Halogen component. But you won't find a single render function in the code. Instead, with a few helper functions, you can write your own `#!hs State -> HTML` function however you'd like. You can:

* Extend the component's functionality by embedding new queries in the HTML
* Extend the component's data by including as much additional state from the parent as you want (which you can then use in your render function)
* Leverage the provided features for user interaction, state management, accessibility, and logic
* Retain complete freedom over the design and aesthetic of your selection component

!!! tip "For visual learners"
    I gave a talk at the Los Angeles PureScript meetup in April 2018 about the approach this library takes. It provides lots of additional context. The talk starts at **35:18**:

    <iframe width="560" height="315" src="https://www.youtube.com/embed/igWrktC0m7E?rel=0&amp;showinfo=0&amp;start=2119" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>
