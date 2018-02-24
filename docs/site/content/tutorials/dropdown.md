+++
title = "Dropdown Tutorial"
description = "Some description"
template = "article.html"
+++

# Some Text

I've written an article here.

```hs
eval = case _ of
  ToContainer q a -> H.query unit q *> pure a

  HandleContainer m a -> case m of
    C.Emit q -> eval q *> pure a

    C.ItemSelected item -> do
      H.modify \st -> st { selected = ( item : st.selected ) }
      st <- H.get
      _  <- H.query unit
              $ H.action
              $ C.ContainerReceiver
              $ { render: renderContainer
                , items: difference st.items st.selected }
      pure a

  Removed item a -> do
    st <- H.get
    let newSelections = delete item st.selected
        newItems = difference newSelections st.items
    H.modify (_ { selected = newSelections })
    _  <- H.query unit
            $ H.action
            $ C.ContainerReceiver
            $ { render: renderContainer
              , items: newItems }
    pure a
```

<br>
## Typeaheads

Support asynchronous, continuously asynchronous, and synchronous typeaheads with autocompletion.

{{ halogen(id="typeahead", header="Typeahead") }}
