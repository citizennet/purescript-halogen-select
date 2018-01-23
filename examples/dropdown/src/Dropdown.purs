module Dropdown where

import Prelude

import Control.Monad.Aff.Console (log, logShow)
import CSS as CSS
import DOM.Event.KeyboardEvent as KE
import Data.Array ((:), difference, mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS as HC
import Select.Effects (FX)
import Select.Primitive.Container as C


type DropdownItem = String

type State =
  { items    :: Array DropdownItem
  , selected :: Array DropdownItem }

data Query a
  = Log String a
  | HandleContainer (C.Message Query DropdownItem) a
  | ToContainer (C.ContainerQuery Query DropdownItem Unit) a

type HTML e = H.ParentHTML Query (C.ContainerQuery Query DropdownItem) Unit (FX e)

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
    initState = { items: testData, selected: [] }

    render :: State -> H.ParentHTML Query (C.ContainerQuery Query DropdownItem) Unit (FX e)
    render st =
      HH.div
        [ HP.class_ $ HH.ClassName "mw8 sans-serif center" ]
        ( [ HH.h2
            [ HP.class_ $ HH.ClassName "black-80 f-headline-1"
            , HE.onKeyDown $ HE.input (Log <<< KE.code)
            ]
            [ HH.text "Menu Component"]
          , HH.p_
            [ HH.text "Open the console to view outputs. "
            , HH.text "Mouse over the toggle to trigger an embedded parent query. "
            , HH.text "Click the toggle to open or close the menu. Click an item to select it (and remove it from the available options)." ]

          , renderToggle
          , HH.slot unit C.component { items: testData, render: renderContainer } (HE.input HandleContainer) ]
          <> selected )
      where
        selected = if st.selected == [] then [] else
          [ HH.h2
              [ HP.class_ $ HH.ClassName "black-80 mt5 f-headline-1" ]
              [ HH.text "Selected Items"]
          , HH.ul_
              ( map (\i -> HH.li_ [ HH.text i ]) st.selected )
          ]

    -- Here, Menu.Emit recursively calls the parent eval function.
    -- Menu.Selected item is handled by removing that item from
    -- the options and maintaining it here in state.
    eval :: Query ~> H.ParentDSL State Query (C.ContainerQuery Query DropdownItem) Unit Void (FX e)
    eval = case _ of
      Log s a -> H.liftAff (log s) *> pure a

      ToContainer q a -> H.query unit q *> pure a

      HandleContainer m a -> case m of
        C.Emit q -> eval q *> pure a

        C.ItemSelected item -> do
          H.liftAff $ log ("Selected: " <> item)
          H.modify \st -> st { selected = ( item : st.selected ) }

          st <- H.get
          H.liftAff $ logShow st.selected
          _  <- H.query unit
                  $ H.action
                  $ C.ContainerReceiver { render: renderContainer, items: difference st.items st.selected }
          pure a



{-

HELPERS

-}

-- The parent must provide some input data.
testData :: Array DropdownItem
testData =
  [ "Thomas Honeyman"
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
  , "Riley Gibbs"
  , "THE COOKIE MONSTER DID NOTHING WRONG" ]


-- Render whatever is going to provide the action for toggling the menu. Notably, this is
-- NOT a primitive.
renderToggle =
	HH.span
		( C.getToggleProps ToContainer
			[ HE.onMouseOver $ HE.input_ $ Log "I'm the parent."
			, HP.class_      $ HH.ClassName "f5 link ba bw1 ph3 pv2 mb2 dib near-black pointer outline-0"
			]
		)
		[ HH.text "Toggle" ]

-- Render function to pass to the child container component
renderContainer :: (C.ContainerState DropdownItem) -> H.HTML Void (C.ContainerQuery Query DropdownItem)
renderContainer st =
  HH.div_
	$ if not st.open
			then [ ]
			else [ renderItems $ renderItem `mapWithIndex` st.items ]
  where
		-- Render the container for the items
		renderItems :: Array (H.HTML Void (C.ContainerQuery Query DropdownItem)) -> H.HTML Void (C.ContainerQuery Query DropdownItem)
		renderItems html =
			HH.div
				( C.getContainerProps
					[ HP.class_ $ HH.ClassName "measure ba br1 b--black-30 overflow-y-scroll pb3 outline-0"
					, HC.style $ CSS.maxHeight (CSS.px 300.0)
					]
				)
				[ HH.div
						[ HP.class_ $ HH.ClassName "cf" ]
						[ HH.h4
							[ HP.class_ $ HH.ClassName "ph2 pv3 ma0 fl w-50" ]
							[ HH.text "Choose One" ]
						, HH.div
								[ HP.class_ $ HH.ClassName "fl w-50 tr" ]
								[ HH.button
									( C.getChildProps
										[ HP.class_ $ HH.ClassName "ma2 ba bw1 ph3 pv2 dib b--near-black pointer outline-0 link"
										, HE.onClick $ HE.input_ $ C.Raise $ H.action $ Log "button in container clicked"
										]
									)
									[ HH.text "Click Me" ]
								]
						]
				, HH.ul
						[ HP.class_ $ HH.ClassName "list pl0 mt0 bt b--black-30" ]
						html
				]

		renderItem :: Int -> DropdownItem -> H.HTML Void (C.ContainerQuery Query DropdownItem)
		renderItem index item = HH.li item' [ HH.text item ]
			where
				item' = C.getItemProps index
					[ HP.class_ $ HH.ClassName
						$ "lh-copy pa2 ba bl-0 bt-0 br-0 b--dotted b--black-30"
						<> if st.highlightedIndex == Just index then " bg-light-blue" else "" ]
