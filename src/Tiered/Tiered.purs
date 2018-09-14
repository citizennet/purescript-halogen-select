module Select.Tiered where

import Prelude

import Data.Array (mapWithIndex)
import Data.Foldable (elem, null)
import Data.List (List, elemIndex, findIndex, updateAt, (:))
import Data.Maybe (Maybe(..), isJust)
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
type Menu item =
  { items :: Array (Item item)
  , highlightedIndex :: Maybe Int
  }

mkMenu :: ∀ item. Array (Item item) -> Menu item
mkMenu items = { items, highlightedIndex: Nothing }

-- | The component state maintains information about the overall
-- | component; collections maintain additional information.
type State item =
  { inputElement :: Maybe HTMLElement
  , stack :: Stack item
  , items :: Array (Item item)
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
  | Highlight (Menu item) Int a

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
  initialState { inputElement, items } = { inputElement, stack: mempty, items }

  css :: ∀ i r. String -> HH.IProp ("class" :: String | r) i
  css = HP.class_ <<< HH.ClassName

  render :: State item -> H.ComponentHTML (Query item)
  render st =
    HH.div
      [ css "container mx-auto p-8" ]
      [ renderToggle
      , renderMenu (mkMenu st.items)
      ]

    where

    renderToggle :: H.ComponentHTML (Query item)
    renderToggle =
      HH.button
        [ HE.onMouseDown $ HE.input \ev -> CaptureRef (ME.toEvent ev)
        , css "bg-blue text-white py-2 px-4 rounded"
        ]
        [ HH.text "Toggle Me Bro" ]

    renderMenu :: Menu item -> H.ComponentHTML (Query item)
    renderMenu menu = if elem menu st.stack
      then HH.ul_ $ mapWithIndex (renderItem menu menu.highlightedIndex) menu.items
      else HH.text ""

    renderItem :: Menu item -> Maybe Int -> Int -> Item item -> H.ComponentHTML (Query item)
    renderItem menu hi ix (Item item) =
      HH.li
        [ if (hi == Just ix) then css "bg-grey-lightest" else css ""
        , HE.onMouseOver $ HE.input_ $ Highlight menu ix
        ]
        [ HH.text item ]
    renderItem menu hi ix i@(Items item items) =
      HH.li
        [ if (hi == Just ix) then css "bg-grey-lightest" else css ""
        , HE.onMouseOver $ HE.input_ $ Highlight menu ix
        ]
        [ HH.div
          [ HE.onMouseDown $ HE.input_ $ SelectItem i ]
          [ HH.text $ item <> " >>>" ]
        , renderMenu (mkMenu items)
        ]

  isMenu :: Item item -> Boolean
  isMenu (Item _) = false
  isMenu (Items _ _) = true

  eval :: Query item ~> H.ComponentDSL (State item) (Query item) (Message item) m
  eval = case _ of
    CaptureRef ev a -> a <$ do
      st <- H.get
      H.modify_ _ { inputElement = fromEventTarget =<< currentTarget ev }
      when (null st.stack) (H.modify_ _ { stack = mkMenu st.items : st.stack })

    SelectItem i a -> a <$ case i of
      Item _ -> H.raise $ Selected i
      Items _ items -> do
        st <- H.get
        let inStack = isJust $ findIndex (\x -> x.items == items) st.stack
        when (not inStack) (H.modify_ _ { stack = mkMenu items : st.stack })
        H.raise $ Selected i

    Highlight menu ix a -> a <$ do
      { stack } <- H.get
      let newMenu = menu { highlightedIndex = Just ix }
      case elemIndex menu stack of
        Nothing -> H.modify_ _ { stack = newMenu : stack }
        Just mix -> case updateAt mix newMenu stack of
          Nothing -> pure unit
          Just newStack -> H.modify_ _ { stack = newStack }


----------
-- Stack Manipulation

type Stack item = List (Menu item)


----------
-- Scratch

{-

1. Maintain focus on some trigger element, but also maintain 'highlight' or 'selected'
   focus either ephemerally (hover) or pinned (click) on menus

2. On items, set handlers for left & right arrows. If this item is an Items, then right
   arrow should open the sub menu, left arrow should do nothing if anything is highlighted
   in the sub menu, left arrow should close the submenu if nothing is highlighted in it.
   If it is an Item, then right arrow should do nothing, left arrow should do nothing.

3. Maintain a stack of "active" menus, where the topmost stack is the one which should
   handle events registered on the focus element

-}
