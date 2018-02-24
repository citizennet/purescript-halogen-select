+++
title = "PureScript Halogen Select"
description = "Some description"
template = "home.html"
+++

## Dropdowns

It's easy to create simple selection components like dropdowns.
{{ halogen(id="dropdown") }}

You'll have to decide how to render your dropdown, but wiring up the selection logic is as easy as this eval function:

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

## Typeaheads

Support asynchronous, continuously asynchronous, and synchronous typeaheads with autocompletion.

{{ halogen(id="typeahead", header="Typeahead") }}

## Calendars

Support keyboard-navigable calendars with Enter to select. Couple it with a search primitive for searchable dates.

{{ halogen(id="calendar", header="Calendar") }}

