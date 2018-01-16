## Module Select.Primitive.Container

#### `Message`

``` purescript
data Message item o e
  = Emit (Dispatch item o e Unit)
  | ItemSelected item
```

The Container sends the parent messages in two instances:
Emit: an embedded query has been triggered, and you must decide how to handle it; typically via evaluating
in the parent or re-routing the query to another primitive.
ItemSelected: an item has been selected from the container.

#### `component`

``` purescript
component :: forall item o e. Component HTML (Dispatch item o e) (ContainerInput item o e) (Message item o e) (FX e)
```

The primitive handles state and transformations but defers all rendering to the parent. The
render function can be written using our helper functions to ensure the right events are included. See the `Dispatch`
module for more information.


