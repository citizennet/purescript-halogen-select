## Module Select.Primitive.Search

#### `Message`

``` purescript
data Message item o e
  = Emit (Dispatch item o e Unit)
  | NewSearch String
```

The Search sends the parent messages in two instances:
Emit: an embedded query has been triggered, and you must decide how to handle it; typically via evaluating
in the parent or re-routing the query to another primitive.
NewSearch: some new text has been searched (this is automatically debounced).

#### `component`

``` purescript
component :: forall item o e. Component HTML (Dispatch item o e) (SearchInput item o e) (Message item o e) (FX e)
```

The primitive handles state and transformations but defers all rendering to the parent. The
render function can be written using our helper functions to ensure the right events are included. See the `Dispatch`
module for more information.


