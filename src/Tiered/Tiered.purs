module Select.Tiered where

import Prelude

import Data.Array (drop, length, (!!), (:))
import Data.Maybe (Maybe(..), isNothing)
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
type Stack item =
  { head :: ActiveMenu item
  , tail :: Array (ParentMenu item)
  }

-- | Make the initial stack from a list of items
initStack :: ∀ item. Array (Item item) -> Stack item
initStack items = { head: mkActiveMenu items, tail: [] }

-- | Push the current highlight onto the stack with a maybe highlighted index
pushStack :: ∀ item. Maybe Int -> Stack item -> Maybe (Stack item)
pushStack hix { head, tail } = do
  phix <- head.highlightedIndex
  activeItems <- case head.items !! phix of
    Just (Items _ items) -> pure items
    _ -> Nothing
  pure
    { head:
      { items: activeItems
      , highlightedIndex: join (flip verifyIndex activeItems <$> hix)
      }
    , tail: { items: head.items, highlightedIndex: phix } : tail
    }

-- |
trimStack :: ∀ item. Int -> Stack item -> Maybe (Stack item)
trimStack n s@{ tail }
  | n <= 0 = Just s
  | otherwise =
      let update x = x { highlightedIndex = Just x.highlightedIndex }
       in case tail !! 0 of
        Nothing -> Nothing
        Just menu -> trimStack (n - 1) { head: update menu, tail: drop 1 tail }


-- |
data HighlightTarget
  = Preview Int
  | Active Int
  | Parent Int Int

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
      --  , renderMenu ((length st.stack) - 1)
      ]

    where

    renderToggle :: H.ComponentHTML (Query item)
    renderToggle =
      HH.button
        [ HE.onMouseDown $ HE.input \ev -> CaptureRef (ME.toEvent ev)
        , css "bg-blue text-white py-2 px-4 rounded"
        ]
        [ HH.text "Toggle Me Bro" ]

    --  renderMenu :: Either PreviewIndex StackIndex -> H.ComponentHTML (Query item)
    --  renderMenu x@(Left (Preview (Stack pix) iix)) = case st.stack !! pix of
    --    Just parentMenu -> case parentMenu !! iix of
    --      Just (Items _ items) -> do
    --        let menu = mkMenu items
    --        HH.ul_ $ mapWithIndex (renderItem x menu.highlightedIndex) menu.items
    --      _ -> HH.text ""
    --    _ -> HH.text ""
    --  renderMenu x@(Right (Stack mix)) = case st.stack !! mix of
    --    Just menu -> HH.ul_ $ mapWithIndex (renderItem x menu.highlightedIndex) menu.items
    --    _ -> HH.text ""

    --  renderItem
    --    :: Either PreviewIndex StackIndex
    --    -> Maybe Int
    --    -> Int
    --    -> Item item
    --    -> H.ComponentHTML (Query item)
    --  renderItem mix hix iix (Item item) =
    --    HH.li
    --      [ if (hix == Just iix) then css "bg-grey-lighter" else css ""
    --      , HE.onMouseOver $ HE.input_ $ Highlight mix iix
    --      ]
    --      [ HH.text item ]
    --  renderItem mix hix iix i@(Items item items) =
    --    HH.li_
    --      [ HH.div
    --        [ if (hix == Just iix) then css "bg-grey-lightest" else css ""
    --        , HE.onMouseOver $ HE.input_ $ Highlight mix iix
    --        ]
    --        [ HH.text $ item <> " >>>" ]
    --      , case mix of
    --          Left (PreviewIndex _ _) -> HH.text ""
    --          Right s | hix == Just iix ->
    --            case nextStackIndex s st.stack of
    --              Just six -> renderMenu (Right six)
    --              Nothing -> HH.text ""
    --      ]

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
        Just stack -> case target of
          Preview iix ->
            H.modify_ _ { stack = pushStack (Just iix) stack }
          Active iix -> do
            let head = stack.head { highlightedIndex = verifyIndex iix stack.head.items }
            H.modify_ _ { stack = Just $ stack { head = head } }
          Parent six iix -> do
            let ns = _ { head { highlightedIndex = Just iix } } <$> trimStack six stack
            H.modify_ _ { stack = ns }

