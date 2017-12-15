module Dispatch where

{-

Centralized routing of events and queries to make writing render functions and effecting behaviors simple.

This module is necessary for several reasons:

1. This library is built so that you can use primitives to build whatever list- or grid-based selection components you want (dropdown, typeahead, image picker, date picker, etc.) rather than export a version of any particular component.

2. Because of that, components aren't self-contained. They are meant to be used with one another. This means mixing query types in HTML, which in Halogen is impossible because of the types involved. Instead, you have to wrap them in one another until you reach some unified query type. That includes any queries from outside this library the user wants to include, wrapped for whatever destination part of the library it's being dispatched to. Naturally, this is hideously tedious.

3. The user is not responsible for behaviors. The user _is_ responsible for a) writing a render function for the component, that b) contains the triggers for the events their select needs to function (though the handlers for those events are defined for you). The main problem with this is that _the various components in this library will be mounted to slots they are unable to query_. That means that something else must handle the machinery of distributing some parent query off to the relevant component that will handle it -- like the input field triggering something in the menu.

Conveniences:

1. The design remains segmented; the typeahead does not duplicate functionality with the menu and a component can be built by sticking the two building blocks together. As the library grows, this should remain the case.

2. Complexity can be masked with helper functions like `inMenu 2 Close` (close the menu in slot 2), and the augmentHTML functions that attach all the proper triggers (though you can also do it manually).

Downsides:

1. The user is forced to use this `Dispatch` module, when they might much prefer to just bring in "an interactive dropdown menu."

2. The user has to write render functions for each primitive anyway, so the configuration begins to grow.

3. It isn't clear this will continue to scale up beyond the very simple examples we have now (or even work now!)

4. Behaviors become more complex. For example, the typeahead input has to send arrow key events up to this Dispatch module, which will send them down to the menu again. This module has to orchestrate all that behavior on behalf of the user. This might make it impossible to actually build this out of blocks and cover all potential combinations.

5. It becomes impossible to just drop a "menu" into your project. Instead, you end up building the component you want out of the pieces we have here for you. It's not clear this is useful.

-}

import Prelude
import Select.Menu as M
import Select.Typeahead as T

data Query item o s i a
  = ParentQuery (o Unit) a
  | Dispatch    (SubQuery item o s i) a

data SubQuery item o s i
  = MenuQuery      (M.Query item o Unit) (s i)
  | TypeaheadQuery (T.Query item o Unit) (s i)

-- The container will necessarily have multiple child query types
-- because of the use of primitives. I'd assume we'd need to encode
-- those into the subquery so you can use the H.query function to
-- actually dispatch it; if the child is not aware of what slot they
-- are allowed to call, this breaks down.
data SlotType i
  = MenuSlot i
  | TypeaheadSlot i

{-

This HTML is a bit tedious, but would allow you to close the menu in slot 2...

  H.event $ H.input $ Dispatch $ MenuQuery M.Close $ MenuSlot 2

We can define helper functions like...

  inMenu i q = Dispatch $ MenuQuery q $ MenuSlot i

Resulting in...

  H.event $ H.input $ inMenu 2 M.Close

Which can be hidden further with...

  getItemProps [ M.embed YourQuery ]

-}


{-

The slot types are necessary because ultimately this ends up in `eval`:

  eval = case _ of
    Dispatch (MenuQuery query (slot id)) -> do
       _ <- H.query (slot id) $ H.action query

I'm unable to dispatch queries to particular slots unless I know about them, and the various components will not know about each other.

-}
