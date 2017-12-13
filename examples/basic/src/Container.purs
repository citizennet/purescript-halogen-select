module Container where

import Prelude

import Select.Dropdown as Dropdown
import Control.Monad.Aff.Console (log)
import Data.Array (mapWithIndex)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | This module holds a default parent component with minimal functionality as
-- | a demonstration.

data Query a = Handle Dropdown.Message a

-- Test data to filter against
testData :: M.Map Int String
testData = M.fromFoldable
  $ mapWithIndex (\i s -> Tuple i s)
  $ [ "Thomas Honeyman"
    , "Dave Zuch"
    , "Chris Cornwell"
    , "Forest Toney"
    , "Lee Leathers"
    , "Kim Wu"
    , "Rachel Blair"
    , "Tara Strauss"
    , "Sanket Sabnis"
    , "Aaron Chu"
    , "Vincent Busam"
    , "Riley Gibbs" ]

-- This component only renders child components, so its `State` and `Message` types
-- are `Unit` and `Void`. Effect types are elided for convenience.
component :: H.Component HH.HTML Query Unit Void _
component =
  H.parentComponent
    { initialState: id
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: Unit -> H.ParentHTML Query Dropdown.Query Unit _
    render _ =
      HH.div
        [ HP.class_ $ HH.ClassName "mw8 sans-serif center" ]
        [ HH.h2
          [ HP.class_ $ HH.ClassName "black-80 f-headline-1" ]
          [ HH.text "Dropdown Component"]
        , HH.slot unit Dropdown.component unit (HE.input Handle)
        ]

    eval :: Query ~> H.ParentDSL Unit Query Dropdown.Query Unit Void _
    eval = case _ of
      Handle m a -> pure a
