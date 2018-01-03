module Calendar where

import Prelude

import Control.Monad.Aff.Console (log)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select.Dispatch (Dispatch(ParentQuery), emit)
import Select.Dispatch as D
import Select.Effects (FX)
import Select.Primitive.Container as C


{-

The calendar component is an example.

-}

data Query a
  = HandleContainer (C.Message String Query) a

type State =
  { items    :: Array (D.Item String)
  , selected :: Array String }

component :: ∀ e. H.Component HH.HTML Query Unit Void (FX e)
component =
  H.parentComponent
    { initialState: const initState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initState :: State
    initState = { items: [], selected: [] }

    render :: State -> H.ParentHTML Query (Dispatch String Query) Unit (FX e)
    render st =
      HH.div
        [ HP.class_ $ HH.ClassName "mw8 sans-serif center" ]
        [ HH.h2
          [ HP.class_ $ HH.ClassName "black-80 f-headline-1" ]
          [ HH.text "Calendar Component"]
        , HH.slot unit (C.component renderContainer) { items: [] } (HE.input HandleContainer)
        ]

    eval :: Query ~> H.ParentDSL State Query (Dispatch String Query) Unit Void (FX e)
    eval = case _ of
      HandleContainer m a -> case m of
        C.Emit q -> emit eval q a

        -- The only other message raised by the container primitive is when an item has been
        -- selected.
        C.ItemSelected item -> a <$ do
          H.liftAff $ log ("Selected! Choice was " <> item)


{-

CONFIGURATION

-}

-- The user is using the Container primitive, so they have to fill out a Container render function
renderContainer :: (C.State String) -> H.HTML Void (Dispatch String Query)
renderContainer st =
  HH.div_
    $ if not st.open
      then [ HH.text "Closed." ]
      else [ HH.text "Not implemented." ]
