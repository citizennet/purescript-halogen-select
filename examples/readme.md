# Examples

These examples demonstrate the component in various configurations, notably with and without typeahead functionality.


## Configuration

The `bower.json` file for each example contains a link to this GitHub repository to install the component. You can run these commands inside the directory of any example:

```sh
npm install
bower install
pulp build --to dist/app.js
```

Then, open the `/dist/index.html` file from within the same directory to view the result.


## Local Development

If you would like to work on the component locally and see your changes reflected in the examples directory, then you should use Bower's link functionality. From the root folder, run:

```sh
bower link
cd /examples/<chosen-example>
bower link purescript-halogen-typeahead
```

Now, any changes to the component will be reflected in the example.
