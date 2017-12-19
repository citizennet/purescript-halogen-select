# Examples

Select is designed as a group of primitives that can be combined together into a variety of selection user interfaces in Halogen. As such, the source itself is not particularly testable and does not demonstrate how to use any of the primitives in practice. The examples folder helps solve both problems:

* Every primitive is involved in at least one example component and all example components are tested with continuous integration.
* Examples demonstrate how to use every primitive and provide implementations that can be copy / pasted into existing Halogen projects.


### Testing Structure

Examples are set up in a specific way so they can be compliant with continuous integration and testing. Most importantly:

1. Each example should be buildable from the root. That means that if you bring in any additional dependencies to build an example, _you must add that dependency to the `devDependencies` field in the root `bower.json` file_. This is in addition to including it in the example's `bower.json` file. This is done so that all examples can share the same `bower_components`, `node_modules`, and `output` folders when being built.

2. Each example should have a build script in the root `package.json` that exactly corresponds to its folder name + the prefix "example-". For example, the `dropdown` example that lives in the `/examples/dropdown` folder should have a build & test script in NPM named `"example-dropdown"`. This is done so that Circle CI can iterate through the `/examples/` directory and run the corresponding NPM script in testing. If you add a new example without a new build script, your build will fail.

3. Circle CI builds using the Dockerfile in `./circleci/images/Dockerfile`. If you bring in an additional dependency that needs to exist in the operating system, you will need to update the dockerfile to do so.

4. You can run and build all examples from the root with the npm script `npm run all-examples`

Build scripts, for now, are simply `pulp build --include examples/<example>/src --to examples/<example>/dist/example.js`. When we build more tests, like snapshot tests, we'll add another command to run tests and use that in Circle CI instead.


### Running Examples From The Root

You can run any example from the home directory with this command:

```sh
# Use the folder name with 'example' pre-pended
npm run <example-name>

# For example, to run the dropdown example:
npm run example-dropdown
```

Then, open the `index.html` file within the `dist` folder for that example:

```sh
# To open the dropdown example:
open ./examples/dropdown/dist/index.html
```

Alternately, you can run all examples at once (they all share the same npm, bower, and output directories):

```sh
npm run all-examples
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

However, you rarely want to do this. It is better to simply build from the root.


# Local Development

When you develop locally, you will need to force Bower to use a local copy of the project instead of this repository. If you don't do this, then your local changes will not be reflected in the `/examples/` directory, and you won't be able to tell if you've broken anything. 

To do that, we'll use Bower's `link` command:

```sh
# In the root folder:
bower link
```

Now, Bower will use the local package as the installation source. You can build & watch any example from the root with this command:

```sh
# Replace <example> with the name of your example
pulp -w build --include examples/<example>/src --to examples/<example>/dist/example.js

# To just build once, you can use this:
npm run example-<example>
```

A typical development process involves:

```sh
# Clone the project
git clone git@github.com:citizennet/purescript-halogen-select.git select

# Build the root
npm install && bower install

# Link to bower
bower link

# Build and watch the source for the primitives
pulp -w build

# If you will be working with an example, build the example
pulp -w build --include examples/<example>/src --to examples/<example>/dist/example.js

# When you have made changes to the primitives and want to ensure the project
# still builds correctly, run this from the root. If these all pass, you'll most likely
# pass CI also.
npm run all-examples
```
