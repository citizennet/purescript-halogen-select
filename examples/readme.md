# Examples

Select is designed as a group of primitives that can be combined together into a variety of selection user interfaces in Halogen. As such, the source itself is not particularly testable and does not demonstrate how to use any of the primitives in practice. The examples folder helps solve both problems:

* Every primitive is involved in at least one example component and all example components are tested with continuous integration.
* Examples demonstrate how to use every primitive and provide implementations that can be copy / pasted into existing Halogen projects.


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
