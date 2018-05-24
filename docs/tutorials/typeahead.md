# Let's build a typeahead!

Typeaheads are among the most common selection components you'll build. Most web developers have had to implement at least one of these before and they can be surprisingly difficult to build. Luckily, with `Select`, implementing a typeahead that fits your custom design takes little more than writing the rendering code and then tweaking it with a helper function or two.

## Conclusion

### Next Steps

Now that you're able to build a typeahead with `Select` you know everything you need to build more complex components like date pickers and image pickers. The next tutorial in the series, *Let's build a date picker*, is currently a work in progress.

!!! tip
    Did you notice anything you would improve about this tutorial or the `Select` library? I'd love to hear about it! Feel free to reach out on the [functional programming Slack](https://functionalprogramming.slack.com/) or on the [PureScript user forum](https://purescript-users.ml). If you found a bug or would like to make an improvement, please open an issue or pull request on the library.

### Source Code

If you'd like to use this component as a starting point from which to build your own, feel free to copy/paste the source code below.

??? article "Full source code for the tutorial"
    ```hs
    module Component where

    import Prelude

    import Control.Monad.Aff (Aff, Milliseconds(..))
    import Control.Monad.Aff.AVar (AVAR)
    import Control.Monad.Aff.Class (class MonadAff)
    import Control.Monad.Aff.Console (CONSOLE)
    import DOM (DOM)
    import Data.Argonaut (Json, decodeJson, (.?))
    import Data.Array (difference, filter, length, mapWithIndex, (:))
    import Data.Either (Either(..))
    import Data.Maybe (Maybe(..))
    import Data.String (Pattern(..), contains)
    import Data.Traversable (traverse)
    import Halogen as H
    import Halogen.HTML as HH
    import Halogen.HTML.Events as HE
    import Halogen.HTML.Properties as HP
    import Network.HTTP.Affjax (AJAX, get)
    import Network.RemoteData (RemoteData(..), withDefault)
    import Select as Select
    import Select.Utils.Setters as Setters

    data Query a
      = HandleSelect (Select.Message Query String) a
      | Remove String a

    type State eff =
      { items :: RemoteData String (Array String)
      , selections :: Array String
      , debounceTime :: Milliseconds
      , fetchItems :: String -> Aff eff (RemoteData String (Array String))
      }

    type Input = Unit

    data Message
      = SelectionChange (Array String)

    type ChildSlot = Unit
    type ChildQuery eff = Select.Query Query String eff

    type Effects eff =
      ( dom :: DOM
      , avar :: AVAR
      , ajax :: AJAX
      , console :: CONSOLE
      | eff
      )

    component :: ∀ eff m
      . MonadAff (Effects eff) m
     => H.Component HH.HTML Query Input Message m
    component =
      H.parentComponent
        { initialState
        , render
        , eval
        , receiver: const Nothing
        }
      where

      initialState :: Input -> State (Effects eff)
      initialState = const
        { items: NotAsked
        , selections: []
        , debounceTime: Milliseconds 100.0
        , fetchItems
        }

      fetchItems
        :: String
        -> Aff (Effects eff) (RemoteData String (Array String))
      fetchItems str = do
         (res :: Json) <- _.response
           <$> get ("https://swapi.co/api/people/?search=" <> str)

         let items = do
               obj <- decodeJson res
               arr <- obj .? "results"
               traverse (decodeJson <=< flip (.?) "name") arr

         pure $ case items of
           Left err -> Failure $ "Could not fetch data:\n\n" <> err
           Right xs -> Success xs

      render
        :: State (Effects eff)
        -> H.ParentHTML Query (ChildQuery (Effects eff)) ChildSlot m
      render parentState =
        HH.div_
        [ HH.h1_
          [ HH.text "Typeahead" ]
        , HH.slot unit Select.component selectInput (HE.input HandleSelect)
        ]
        where
          selectInput :: Select.Input Query String (Effects eff)
          selectInput =
            { inputType: Select.TextInput
            , items: withDefault [] parentState.items
            , initialSearch: Nothing
            , debounceTime: Just parentState.debounceTime
            , render: typeahead
            }

          typeahead
            :: Select.State String (Effects eff)
            -> Select.ComponentHTML Query String (Effects eff)
          typeahead childState =
            HH.div_
            [ HH.ul_
              ( parentState.selections <#>
                (\item ->
                    HH.li
                    [ HE.onClick $ Select.always $ Select.raise $ Remove item unit ]
                    [ HH.text item ]
                )
              )
            , HH.input
              ( Setters.setInputProps [] )
            , case childState.visibility of
                Select.Off -> HH.text ""
                Select.On -> HH.ul (Setters.setContainerProps []) $
                  case length childState.items of
                    0 ->
                      [ HH.li
                        [ HE.onClick
                          $ Select.always
                          $ Select.raise
                          $ HandleSelect (Select.Searched "") unit ]
                        [ HH.text "Fetch new data" ]
                      ]
                    _ -> []
                  <>
                  ( mapWithIndex
                    (\ix item ->
                      HH.li
                        ( Setters.setItemProps ix
                          $ case Just ix == childState.highlightedIndex of
                              true -> [ HP.attr (HH.AttrName "style") "color: red;" ]
                              _ -> [] )
                        [ HH.text item ]
                    )
                    childState.items
                  )
            ]

      eval
        :: Query
        ~> H.ParentDSL (State (Effects eff)) Query (ChildQuery (Effects eff)) ChildSlot Message m
      eval = case _ of
        Remove item next -> do
           H.modify \st -> st { selections = filter (_ /= item) st.selections }
           st <- H.get
           _ <- H.query unit
             $ Select.replaceItems
             $ difference (withDefault [] st.items) st.selections
           pure next

        HandleSelect message next -> case message of
          Select.Searched string -> do
            st <- H.get

            H.modify _ { items = Loading }
            _ <- H.query unit $ Select.replaceItems []
            newItems <- H.liftAff (st.fetchItems string)
            H.modify _ { items = newItems }

            _ <- H.query unit
              $ Select.replaceItems
              $ filter (contains (Pattern string))
              $ difference (withDefault [] newItems) st.selections

            pure next

          Select.Selected item -> do
            H.modify \st -> st { selections = item : st.selections }
            st <- H.get
            _ <- H.query unit
              $ Select.replaceItems
              $ difference (withDefault [] st.items) st.selections
            H.raise $ SelectionChange st.selections
            pure next

          Select.VisibilityChanged vis ->
            pure next

          Select.Emit query -> do
            eval query
            pure next
    ```
