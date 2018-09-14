module Docs.Components.Tiered where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Select.Tiered (Item(..))
import Select.Tiered as Tiered

data Query a
  = HandleSelect (Tiered.Message String) a

data Message = Void

type ChildSlot = Unit
type ChildQuery = Tiered.Query String

component :: ∀ m. MonadAff m => H.Component HH.HTML Query Unit Message m
component =
  H.parentComponent
    { initialState: identity
    , render
    , eval
    , receiver: const Nothing
    }
  where

    eval :: Query ~> H.ParentDSL Unit Query ChildQuery ChildSlot Message m
    eval = case _ of
      HandleSelect _ a -> pure a

    render _ =
      HH.slot unit Tiered.component
        { inputElement: Nothing, items }
        ( HE.input HandleSelect )

    items :: Array (Item String)
    items =
      [ Item "Companies"
      , Item "Accounts"
      , Item "Social Identities"
      , Items "Sub Menu"
          [ Item "Facebook"
          , Item "Twitter"
          , Item "Google+"
          , Item "Pinterest"
          , Item "LinkedIn"
          , Items "Other"
              [ Item "Diaspora"
              , Item "Ello"
              , Item "Discourse"
              ]
          ]
      ]

