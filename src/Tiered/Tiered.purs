module Select.Tiered where

import Prelude

import Data.Array (drop, length, mapWithIndex, (!!), (:))
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..), snd)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event, currentTarget)
import Web.HTML.HTMLElement (HTMLElement, fromEventTarget)
import Web.UIEvent.MouseEvent as ME

-- | A data type representing the possibility of tiers. An item may
-- | be a single item or a possible item node with a further collection.
-- |
-- | If you need to differentiate between items that are terminal an items
-- | that contain further collections, make sure to include that in the
-- | `item` data type.
data Item item
  = Item String
  | Items String (Array (Item item))
derive instance eqItems :: Eq item => Eq (Item item)
derive instance ordItems :: Ord item => Ord (Item item)

-- | A data type representing a collection of items, which may
-- | contain further collections
type ActiveMenu item =
  { items :: Array (Item item)
  , highlightedIndex :: Maybe Int
  }

mkActiveMenu :: ∀ item. Array (Item item) -> ActiveMenu item
mkActiveMenu items = { items, highlightedIndex: Nothing }

-- | A data type representing a collection of items, which may
-- | contain further collections
type ParentMenu item =
  { items :: Array (Item item)
  , highlightedIndex :: Int
  }

mkParentMenu :: ∀ item. Int -> Array (Item item) -> ParentMenu item
mkParentMenu highlightedIndex items = { highlightedIndex, items }

-- | The stack consists of an active menu (head) that key actions will
-- | operate on, plus one or more parent menus.
type Stack item = Tuple (ActiveMenu item) (Array (ParentMenu item))

-- | Make the initial stack from a list of items
initStack :: ∀ item. Array (Item item) -> Stack item
initStack items = Tuple (mkActiveMenu items) []

-- | Push the current highlight onto the stack with a maybe highlighted index
pushStack :: ∀ item. Maybe Int -> Stack item -> Maybe (Stack item)
pushStack hix (Tuple head tail) = do
  phix <- head.highlightedIndex
  items <- case head.items !! phix of
    Just (Items _ items) -> pure items
    _ -> Nothing
  let head' = { items, highlightedIndex: join (flip verifyIndex items <$> hix) }
  pure $ Tuple head' ({ items: head.items, highlightedIndex: phix } : tail)

-- |
trimStack :: ∀ item. StackIndex -> Stack item -> Maybe (Stack item)
trimStack (StackIndex n) s@(Tuple _ tail)
  | n <= 0 = Just s
  | otherwise =
      let update x = x { highlightedIndex = Just x.highlightedIndex }
       in case tail !! 0 of
        Nothing -> Nothing
        Just menu -> trimStack (StackIndex $ n - 1) (Tuple (update menu) (drop 1 tail))

-- |
data HighlightTarget
  = InPreview ItemIndex
  | InActive ItemIndex
  | InParent StackIndex ItemIndex

newtype StackIndex = StackIndex Int
derive instance newtypeStackIndex :: Newtype StackIndex _
derive instance eqStackIndex :: Eq StackIndex

newtype ItemIndex = ItemIndex Int
derive instance newtypeItemIndex :: Newtype ItemIndex _
derive instance eqItemIndex :: Eq ItemIndex

-- |
data MenuTier
  = Preview
  | Stack StackIndex

-- | Verify the provided integer is in bounds for the array
verifyIndex :: ∀ a. Int -> Array a -> Maybe Int
verifyIndex n arr
  | n >= 0 && n < length arr = Just n
  | otherwise = Nothing

-- | The component state maintains information about the overall
-- | component; collections maintain additional information.
type State item =
  { inputElement :: Maybe HTMLElement
  , items :: Array (Item item)
  , stack :: Maybe (Stack item)
  }

-- | Can provide an optional input element for simplicity, otherwise
-- | it will be captured automatically. Provide the initial items.
type Input item =
  { inputElement :: Maybe HTMLElement
  , items :: Array (Item item)
  }

-- | The parent is notified when an item is selected
data Message item
  = Selected (Item item)

-- | Common actions that are supported by the component
data Query item a
  = CaptureRef Event a
  | SelectItem (Item item) a
  | Highlight HighlightTarget a

-- | The component
component
  :: ∀ item m
   . MonadAff m
  => Eq item
  => H.Component HH.HTML (Query item) (Input item) (Message item) m
component =
  H.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }

  where

  initialState :: Input item -> State item
  initialState { inputElement, items } = { inputElement, stack: Nothing, items }

  css :: ∀ i r. String -> HH.IProp ("class" :: String | r) i
  css = HP.class_ <<< HH.ClassName

  render :: State item -> H.ComponentHTML (Query item)
  render st =
    HH.div
      [ css "container mx-auto p-8" ]
      [ renderToggle
      , case st.stack of
          Nothing -> HH.text "nothing in the stack"
          Just x -> renderMenu (Stack $ StackIndex $ length $ snd x)
      ]

    where

    renderToggle :: H.ComponentHTML (Query item)
    renderToggle =
      HH.button
        [ HE.onMouseDown $ HE.input \ev -> CaptureRef (ME.toEvent ev)
        , css "bg-blue text-white py-2 px-4 rounded"
        ]
        [ HH.text "Toggle Me Bro" ]

    renderMenu :: MenuTier -> H.ComponentHTML (Query item)
    renderMenu tier = case tier, st.stack of
      _, Nothing -> HH.text "there is no stack"

      i@(Stack (StackIndex 0)), Just (Tuple menu _) ->
        HH.ul_
          $ flip mapWithIndex menu.items
          $ (\ix -> renderItem i menu.highlightedIndex (ItemIndex ix))

      i@(Stack (StackIndex n)), Just (Tuple _ tail)
        | n > 0 -> case tail !! (n - 1) of
          Nothing -> HH.text ""
          Just menu ->
            HH.ul_
              $ flip mapWithIndex menu.items
              $ (\ix -> renderItem i (Just menu.highlightedIndex) (ItemIndex ix))
        | otherwise -> HH.text ""

      Preview, Just (Tuple head _) -> case head.highlightedIndex >>= (!!) head.items of
        Just (Items _ items) ->
          HH.ul_
            $ flip mapWithIndex items
            $ (\ix -> renderItem Preview Nothing (ItemIndex ix))
        _ -> HH.text ""


    renderItem
      :: MenuTier -> Maybe Int -> ItemIndex -> Item item -> H.ComponentHTML (Query item)
    renderItem tier hix iix i@(Item item) =
      HH.li
        [ if hix == Just (unwrap iix) then css "bg-grey-lighter" else css ""
        , HE.onMouseOver $ HE.input_ $ Highlight $ case tier of
            Preview -> InPreview iix
            Stack (StackIndex 0) -> InActive iix
            Stack six -> InParent six iix
        ]
        [ HH.text item ]
    renderItem tier hix iix i@(Items item items) =
      HH.li_
        [ HH.div
          [ if hix == Just (unwrap iix) then css "bg-grey-lighter" else css ""
          , HE.onMouseOver $ HE.input_ $ Highlight $ case tier of
              Preview -> InPreview iix
              Stack (StackIndex 0) -> InActive iix
              Stack six -> InParent six iix
          ]
          [ HH.text $ item <> " >>>" ]
        , case hix == Just (unwrap iix), tier of
            true, Stack (StackIndex n)
              | n == 0 -> renderMenu Preview
              | n >= 0 -> renderMenu (Stack (StackIndex $ n - 1))
            _, _ -> HH.text ""
        ]

  eval :: Query item ~> H.ComponentDSL (State item) (Query item) (Message item) m
  eval = case _ of
    CaptureRef ev a -> a <$ do
      st <- H.get
      H.modify_ _ { inputElement = fromEventTarget =<< currentTarget ev }
      when (isNothing st.stack) (H.modify_ _ { stack = Just $ initStack st.items })

    SelectItem item a -> do
      H.raise $ Selected item
      pure a

    Highlight target a -> a <$ do
      st <- H.get
      case st.stack of
        Nothing -> pure unit
        Just stack@(Tuple head tail) -> case target of
          InPreview (ItemIndex iix) ->
            H.modify_ _ { stack = pushStack (Just iix) stack }
          InActive (ItemIndex iix) -> do
            let head' = head { highlightedIndex = verifyIndex iix head.items }
            H.modify_ _ { stack = Just $ lmap (const head') stack }
          InParent six (ItemIndex iix) -> do
            let ns = lmap (_ { highlightedIndex = Just iix }) <$> trimStack six stack
            H.modify_ _ { stack = ns }

