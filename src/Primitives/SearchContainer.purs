module Select.Primitives.SearchContainer where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import DOM (DOM)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe)
import Data.Time.Duration (Milliseconds)
import Halogen as H
import Halogen.Component.ChildPath (cp1, cp2) as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Select.Primitives.Container as C
import Select.Primitives.Search as S

data SearchContainerQuery o item eff a
  = ToSearch (S.SearchQuery o item eff Unit) a
  | ToContainer (C.ContainerQuery o item Unit) a
  | HandleSearch (S.Message o item) a
  | HandleContainer (C.Message o item) a
  | Receiver (Input o item eff) a

type State = Unit

type Input o item eff =
  { items :: Array item
  , search :: Maybe String
  , debounceTime :: Milliseconds
  , renderSearch :: S.SearchState eff -> H.ComponentHTML (S.SearchQuery o item eff)
  , renderContainer :: C.ContainerState item -> H.ComponentHTML (C.ContainerQuery o item)
  }

data Message o item
  = ContainerMessage (C.Message o item)
  | SearchMessage (S.Message o item)
  | Emit (o Unit)

type Effects eff = ( dom :: DOM, avar :: AVAR | eff )

type ChildQuery o item eff = Coproduct2
  (C.ContainerQuery o item)
  (S.SearchQuery    o item eff)
type ChildSlot = Either2 Slot Slot

data Slot
  = ContainerSlot
  | SearchSlot
derive instance eqPrimitiveSlot :: Eq Slot
derive instance ordPrimitiveSlot :: Ord Slot

type StateStore o item eff m =
  Store
    State
    (H.ParentHTML
      (SearchContainerQuery o item eff)
      (ChildQuery o item eff)
      ChildSlot
      m)

component :: ∀ o item eff m
  . MonadAff (Effects eff) m
 => H.Component
     HH.HTML
     (SearchContainerQuery o item (Effects eff))
     (Input o item (Effects eff))
     (Message o item)
     m
component =
  H.parentComponent
    { initialState
    , render: extract
    , eval
    , receiver: HE.input Receiver
    }
  where
    initialState
      :: Input o item (Effects eff)
      -> StateStore o item (Effects eff) m
    initialState i = store render' unit
      where
        render' _ =
          HH.div_
          [ HH.slot' CP.cp2 SearchSlot S.component inputS (HE.input HandleSearch)
          , HH.slot' CP.cp1 ContainerSlot C.component inputC (HE.input HandleContainer) ]

        inputS =
          { search: i.search
          , debounceTime: i.debounceTime
          , render: i.renderSearch }

        inputC =
          { items: i.items
          , render: i.renderContainer }


    ----------
    -- EVAL

    eval
      :: SearchContainerQuery o item (Effects eff)
      ~> H.ParentDSL
          (StateStore o item (Effects eff) m)
          (SearchContainerQuery o item (Effects eff))
          (ChildQuery o item (Effects eff))
          (ChildSlot)
          (Message o item)
          m

    -- Route any container queries to the container
    eval (ToContainer q a) = H.query' CP.cp1 ContainerSlot q *> pure a

    -- Route any search queries to the search input
    eval (ToSearch q a) = H.query' CP.cp2 SearchSlot q *> pure a

    eval (HandleContainer m a) = case m of
      C.Emit query -> H.raise (Emit query) *> pure a
      other -> H.raise (ContainerMessage other) *> pure a

    eval (HandleSearch m a) = case m of
      S.Emit query -> H.raise (Emit query) *> pure a
      other -> H.raise (SearchMessage other) *> pure a

    eval (Receiver i a) = do
      _ <- H.query' CP.cp1 ContainerSlot $ H.action $ C.ContainerReceiver containerInput
      _ <- H.query' CP.cp2 SearchSlot $ H.action $ S.SearchReceiver searchInput
      pure a
      where
        searchInput =
          { search: i.search
          , debounceTime: i.debounceTime
          , render: i.renderSearch }
        containerInput =
          { items: i.items
          , render: i.renderContainer }

