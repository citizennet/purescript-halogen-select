+++
title = "Functions"
description = "Some description"
template = "article.html"
+++

<!-- generated via http://www.tablesgenerator.com/markdown_tables -->

<!-- https://docs.google.com/spreadsheets/d/1JSFWVPiLFQE4xkf8KEMFzSWH3C3pdACBzNQLfhOTasg/edit?usp=sharing -->


| primitive | function                                                                                                                                                 | description                                                                                                                                                                              |
|-----------|----------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Search    | `getInputProps`                                                                                                                                          | Use on the array of properties for the input field you want to use to capture user input                                                                                                 |
| Container | `getItemProps`           | Attach events to an item in the container to support selection, highlighting, and key events (like Enter to select).                                                                      |
| Container | `getContainerProps` | Attach properties to the HTML element that encloses the container. This makes sure you can select items without blurring the container, unless you want to.                              |
| Container | `getChildProps`                                                                                                                                          | A helper to embed your own HTML and queries inside the container's render function. It will ensure that your events do not inadvertently steal focus or trigger a blur on the container. |
| Container | `getToggleProps`                                                                                                                                         | Attach properties to a DOM node that will maintain focus and capture key and click events for the container. If you are using the search primitive, this helper is unnecessary.          |
