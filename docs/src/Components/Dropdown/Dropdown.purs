module Docs.Components.Dropdown where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import DOM (DOM)
import Data.Array (difference, mapWithIndex)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select as Select
import Select.Utils.Setters as Setters

type Effects eff = ( avar :: AVAR, dom :: DOM, console :: CONSOLE | eff )
type State = { items :: Array String, text :: String }
type Input = { items :: Array String }
data Query a = HandleSelect (Select.Message Query String) a
data Message = Void

type ChildSlot = Unit
type ChildQuery eff = Select.Query Query String eff

component :: ∀ m e
  . MonadAff ( Effects e ) m
 => H.Component HH.HTML Query Input Message m
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: Input -> State
    initialState i = { items: i.items, text: "Select an option" }

    eval
      :: Query
      ~> H.ParentDSL State Query (ChildQuery (Effects e)) ChildSlot Message m
    eval = case _ of
      HandleSelect (Select.Selected item) a -> do
        st <- H.get
        _ <- H.query unit $ Select.setVisibility Select.Off
        _ <- H.query unit $ Select.replaceItems (difference st.items [ item ])
        H.modify _ { text = item }
        pure a

      HandleSelect other a -> pure a

    render
      :: State
      -> H.ParentHTML Query (ChildQuery (Effects e)) ChildSlot m
    render st =
      HH.div
        [ class_ "w-full" ]
        [ HH.slot unit Select.component input (HE.input HandleSelect) ]
      where
        input =
          { initialSearch: Nothing
          , debounceTime: Nothing
          , inputType: Select.Toggle
          , items: difference st.items [ st.text ]
          , render: renderDropdown
          }

        class_ :: ∀ p i. String -> H.IProp ( "class" :: String | i ) p
        class_ = HP.class_ <<< HH.ClassName

        renderDropdown :: Select.State String (Effects e) -> Select.ComponentHTML Query String (Effects e)
        renderDropdown state = HH.div_ [ renderToggle, renderContainer ]
          where
            renderToggle =
              HH.button
                ( Setters.setToggleProps props )
                [ HH.text st.text ]
                where
                  props = [ class_ "bg-blue hover:bg-blue-dark text-white font-bold py-2 px-4 rounded-sm w-full flex" ]

            renderContainer =
              HH.div [ class_ "relative z-50" ]
              $ if state.visibility == Select.Off
                then []
                else [ renderItems $ renderItem `mapWithIndex` state.items ]
              where
                renderItems html =
                  HH.div
                    ( Setters.setContainerProps props )
                    [ HH.ul [ class_ "list-reset" ] html ]
                  where
                    props = [ class_ "absolute bg-white shadow rounded-sm pin-t pin-l w-full" ]

                renderItem index item =
                  HH.li
                    ( Setters.setItemPropsAndBlur index props )
                    [ HH.text item ]
                  where
                    props = [ class_
                      $ "px-4 py-1 text-grey-darkest"
                      <> if state.highlightedIndex == Just index then " bg-grey-lighter" else ""
                      ]
