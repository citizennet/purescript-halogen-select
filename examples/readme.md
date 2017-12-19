# Examples

Select is designed as a group of primitives that can be combined together into a variety of selection user interfaces in Halogen. As such, the source itself is not particularly testable and does not demonstrate how to use any of the primitives in practice. The examples folder helps solve both problems:

* Every primitive is involved in at least one example component and all example components are tested with continuous integration.
* Examples demonstrate how to use every primitive and provide implementations that can be copy / pasted into existing Halogen projects.


### Testing Structure

Examples are set up in a specific way so they can be compliant with continuous integration and testing. Most importantly:

1. Each example should be buildable from the root. That means that if you bring in any additional dependencies to build an example, _you must add that dependency to the `devDependencies` field in the root `bower.json` file_. This is in addition to including it in the example's `bower.json` file. This is done so that all examples can share the same `bower_components`, `node_modules`, and `output` folders when being built.
2. Each example should have a build script in the root `package.json` that exactly corresponds to its folder name + the prefix "example-". For example, the `basic` example that lives in the `/examples/basic` folder should have a build & test script in NPM named `"example-basic"`. This is done so that Circle CI can iterate through the `/examples/` directory and run the corresponding NPM script in testing. If you add a new example without a new build script, your build will fail.
3. Circle CI builds using the Dockerfile in `./circleci/images/Dockerfile`. If you bring in an additional dependency that needs to exist in the operating system, you will need to update the dockerfile to do so.

Build scripts, for now, are simply `pulp build --include examples/<example>/src --to examples/<example>/dist/example.js`. When we build more tests, like snapshot tests, we'll add another command to run tests and use that in Circle CI instead.


### Running Examples From The Root

You can run any example from the home directory with this command:

```sh
# Use the folder name with 'example' pre-pended
npm run <example-name>

# For example, to run the basic example:
npm run example-basic
```

Then, open the `index.html` file within the `dist` folder for that example:

```sh
# To open the basic example:
open ./examples/basic/dist/index.html
```

### Building A Single Example As A Self-Contained Project

Each example can be run as a standalone project because its `bower.json` file points to this repository. You can run these commands within any of the example folders: 

```sh
npm install && bower install

# equivalent to pulp build --to dist/example.js
npm run build  

# open in the browser:
open ./dist/index.html
```

## Local Development

When you develop locally, you will need to force Bower to use a local copy of the project instead of this repository. If you don't do this, then your local changes will not be reflected in the `/examples/` directory, and you won't be able to tell if you've broken anything. 

To do that, we'll use Bower's `link` command:

```sh
# In the root folder:
# cd <purescript-halogen-select>
bower link

# In an example folder:
# cd ./examples/basic
bower link purescript-halogen-select
```

Now, Bower will use the local package as the installation source.
