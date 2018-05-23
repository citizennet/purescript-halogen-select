# Introduction
Halogen is a powerful framework for building PureScript applications. It’s used by several companies, including SlamData and my own company, CitizenNet (a Conde Nast company), among others. The `Select` library is written for the Halogen framework, so if you don’t know how to use Halogen yet, you ought to start with the [Halogen guide](https://github.com/slamdata/purescript-halogen/tree/master/docs). That said, with only passing familiarity with Halogen, you should be able to follow along just fine!

## Setup
Instead of creating a new Halogen project from scratch, we’ll start with a minimal starter template. The template contains just a single button component, which we’ll modify to work as a dropdown, and then extend to work as a typeahead and more.

First, clone the Halogen template project from CitizenNet, install dependencies, and make sure things build properly. If they don’t, please reach out on the [Purescript user forum](https://purescript-users.ml) so we can fix it!

!!! note ""
    We prefer Yarn over NPM for package management and scripts, but either one will work. Anywhere you see `#!sh yarn <script>`, you can substitute `#!npm run <script>` instead. Feel free to look at the `package.json` file if you want to see what these scripts are doing.

The following steps should get you set up with everything you need to complete these tutorials:

```shell
# Get the CitizenNet starter Halogen project
git clone git@github.com:citizennet/purescript-halogen-template.git

# Change into the directory and install packages
cd purescript-halogen-template && yarn

# Build the project
yarn build

# Open the application in the browser
open dist/index.html
```

After you complete a step in the tutorial, make sure to rebuild the project and refresh your browser to see your updated component.

## Watching for file changes
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
