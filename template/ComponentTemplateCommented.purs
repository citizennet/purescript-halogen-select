module Template.Select.Commented where

import Prelude

import Data.Array (index, length, mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Select as S
import Select.Setters as SS

type Input = Unit

type AddedState =
  ( buttonLabel :: String
  , selection :: Maybe String
  , items :: Array String
  )

data Action
  = DoStuff
  | Initialize
  | Finalize
  | Receive Input

data Query a
  = Reply (Unit -> a)
  | Command a

type Message = Void
type ChildSlots = ()
type Monad = Aff
type SelfSlot index = S.Slot Query ChildSlots Message index

component :: S.Component Query ChildSlots Input Message Monad
component = S.component mkInput $ S.defaultSpec
  { render = render
  , handleAction = handleAction
  , handleQuery = handleQuery
  , handleEvent = handleEvent
  , receive = Just <<< Receive
  , initialize = Just Initialize
  , finalize = Just Finalize
  }
  where
    mkInput :: Input -> S.Input AddedState
    mkInput _ =
      { inputType: S.Toggle
      , search: Nothing
      , debounceTime: Nothing
      , getItemCount: \state -> length state.items

      -- labels from AddedState
      , buttonLabel: "-- Select --"
      , selection: Nothing
      , items: [ "1", "2", "3" ]
      }

    render :: S.State AddedState -> S.ComponentHTML Action ChildSlots Monad
    render state =
      HH.div
        [ HP.class_ $ ClassName "Dropdown" ]
        [ renderToggle, renderContainer ]
      where
      renderToggle =
        HH.button
          ( SS.setToggleProps [ HP.class_ $ ClassName "Dropdown__toggle" ] )
          [ HH.text (fromMaybe state.buttonLabel state.selection) ]

      renderContainer =
        if (state.visibility == S.Off)
          then
            HH.text ""
          else
            HH.div
              ( SS.setContainerProps [ HP.class_ $ ClassName "Dropdown__container" ] )
              ( mapWithIndex renderItem state.items )

      renderItem index item =
        HH.div
          ( SS.setItemProps index
              [ HP.classes $ ClassName <$>
                  [ "Dropdown__item"
                  , if (state.highlightedIndex /= Just index)
                      then ""
                      else "Dropdown__item--highlighted"
                  ]
              ]
          )
          [ HH.text item ]

    handleAction :: Action -> S.HalogenM AddedState Action ChildSlots Message Monad Unit
    handleAction = case _ of
      DoStuff -> do
        pure unit
      Initialize -> do
        pure unit
      Finalize -> do
        pure unit
      Receive input -> do
        pure unit

    handleQuery :: forall a. Query a -> S.HalogenM AddedState Action ChildSlots Message Monad (Maybe a)
    handleQuery = case _ of
      Reply reply -> do
        pure $ Just $ reply unit
      Command next -> do
        pure $ Just next

    handleEvent :: S.Event -> S.HalogenM AddedState Action ChildSlots Message Monad Unit
    handleEvent = case _ of
      S.Searched str -> do
        pure unit
      S.Selected idx -> do
        H.modify_ \s -> s { selection = index s.items idx }
      S.VisibilityChanged S.Off -> do
        pure unit
      S.VisibilityChanged S.On -> do
        pure unit
