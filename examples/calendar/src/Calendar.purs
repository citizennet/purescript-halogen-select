module Calendar where

import Calendar.Utils
import Prelude

import CSS as CSS
import Control.Monad.Aff.Console (log)
import Data.Array (drop, head, last, mapWithIndex, reverse, take, (:), length)
import Data.Date (Date, Day, Month(..), Weekday(..), Year, canonicalDate, day, lastDayOfMonth, month, weekday, year)
import Data.DateTime (adjust, date)
import Data.DateTime.Instant (fromDate, toDateTime)
import Data.Either (fromRight)
import Data.Enum (downFrom, toEnum, upFrom)
import Data.Foldable (foldr)
import Data.Formatter.DateTime (formatDateTime)
import Data.Int (toNumber)
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Monoid (mempty)
import Data.Ord (abs)
import Data.Time.Duration (Days(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (class Unfoldable)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Select.Dispatch (ContainerQuery(..), Dispatch(..), VisibilityStatus(..), embed, emit, getChildProps, getContainerProps, getItemProps, getToggleProps, ContainerState)
import Select.Effects (FX)
import Select.Primitive.Container as C


{-

The calendar component is an example.

-}

data Query e a
  = HandleContainer (C.Message CalendarItem (Query e) e) a
  | ToggleYear  Direction a
  | ToggleMonth Direction a

data Direction = Prev | Next

type ParentHTML e = H.ParentHTML (Query e) (ChildQuery e) Unit (FX e)
type ChildQuery e = Dispatch CalendarItem (Query e) e

type State =
  { targetDate :: Tuple Year Month
  , statuses   :: Array CalendarItem }

data Selection
  = Single Date
  | Range Date Date

-- Calendar Items
data CalendarItem
  = CalendarItem SelectableStatus SelectedStatus BoundaryStatus Date

data SelectableStatus
  = NotSelectable
  | Selectable

data SelectedStatus
  = NotSelected
  | Selected

data BoundaryStatus
  = OutOfBounds
  | InBounds

component :: ∀ e. H.Component HH.HTML (Query e) Unit Void (FX e)
component =
  H.parentComponent
    { initialState: const initState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initState :: State
    initState = { targetDate: Tuple (unsafeMkYear 2018) (unsafeMkMonth 2), statuses: [] }

    eval :: (Query e) ~> H.ParentDSL State (Query e) (ChildQuery e) Unit Void (FX e)
    eval = case _ of
      HandleContainer m a -> case m of
        C.Emit q -> emit eval q a

        -- The only other message raised by the container primitive is when an item has been
        -- selected.
        C.ItemSelected item -> a <$ do
          let showCalendar (CalendarItem _ _ _ d) = show d
          H.liftAff $ log "Selected!"
          H.liftAff $ log ("Selected! Choice was " <> showCalendar item)

      ToggleMonth dir a -> a <$ do
        st <- H.get

        let y = fst st.targetDate
            m = snd st.targetDate

        let (newDate :: Date) = case dir of
               Next -> nextMonth (canonicalDate y m bottom)
               Prev -> prevMonth (canonicalDate y m bottom)

        H.modify _ { targetDate = Tuple (year newDate) (month newDate) }

      ToggleYear dir a -> a <$ do
        st <- H.get

        let y = fst st.targetDate
            m = snd st.targetDate

        let (newDate :: Date) = case dir of
               Next -> nextYear (canonicalDate y m bottom)
               Prev -> prevYear (canonicalDate y m bottom)

        H.modify _ { targetDate = Tuple (year newDate) (month newDate) }

    render :: State -> H.ParentHTML (Query e) (ChildQuery e) Unit (FX e)
    render st =
      HH.div
        [ HP.class_ $ HH.ClassName "mw8 sans-serif center" ]
        [ HH.h2
          [ HP.class_ $ HH.ClassName "black-80 f-headline-1" ]
          [ HH.text "Calendar Component"]
        , HH.slot
            unit
            C.component
            { items: generateCalendarRows targetYear targetMonth
            , render: renderContainer targetYear targetMonth
            }
            ( HE.input HandleContainer )
        ]

      where
        targetYear  = fst st.targetDate
        targetMonth = snd st.targetDate

        renderToggle :: H.HTML Void (ChildQuery e)
        renderToggle =
          HH.span
          ( getToggleProps
            [ HP.class_ $ HH.ClassName "f5 link ba bw1 ph3 pv2 mb2 dib near-black pointer outline-0" ]
          )
          [ HH.text "Toggle" ]

        -- The user is using the Container primitive, so they have to fill out a Container render function
        renderContainer :: Year -> Month -> (ContainerState CalendarItem) -> H.HTML Void (ChildQuery e)
        renderContainer y m cst =
          HH.div_
            $ if not cst.open
              then [ renderToggle ]
              else [ renderToggle
                   , renderCalendar
                   ]

          where
            fmtMonthYear = (unsafePartial fromRight) <<< formatDateTime "MMMM YYYY" <<< toDateTime <<< fromDate
            monthYear = fmtMonthYear (canonicalDate y m bottom)

            renderCalendar :: H.HTML Void (ChildQuery e)
            renderCalendar =
              HH.div
                ( getContainerProps
                  [ HP.class_ $ HH.ClassName "tc"
                  , HC.style  $ CSS.width (CSS.rem 28.0) ]
                )
                [ calendarNav
                , calendarHeader
                , HH.div_ $ renderRows $ rowsFromArray cst.items
                ]

            -- Given a string ("Month YYYY"), creates the calendar navigation
            calendarNav :: H.HTML Void (ChildQuery e)
            calendarNav =
              HH.div
              [ HP.class_ $ HH.ClassName "flex pv3" ]
              [ arrowButton (ToggleYear Prev) "<<" (Just "ml2")
              , arrowButton (ToggleMonth Prev) "<" Nothing
              , dateHeader
              , arrowButton (ToggleMonth Next) ">" Nothing
              , arrowButton (ToggleYear Next) ">>" (Just "mr2")
              ]
              where
                -- Buttons next to the date.
                arrowButton :: H.Action (Query e) -> String -> Maybe String -> _
                arrowButton q t css =
                  HH.button
                  ( getChildProps
                    [ HP.class_ $ HH.ClassName $ "w-10" <> fromMaybe "" (((<>) " ") <$> css)
                    , HE.onClick $ HE.input_ $ embed q ]
                  )
                  [ HH.text t ]

                -- Show the month and year
                dateHeader =
                  HH.div
                  [ HP.class_ $ HH.ClassName "w-60 b" ]
                  [ HH.text monthYear ]

            calendarHeader :: _
            calendarHeader =
              HH.div
              [ HP.class_ $ HH.ClassName "flex pv3" ]
              ( headers <#> (\day -> HH.div [ HP.class_ $ HH.ClassName "w3" ] [ HH.text day ]) )
              where
                headers = [ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" ]

            renderRows :: Array (Array CalendarItem) -> Array (H.HTML Void (ChildQuery e))
            renderRows arr = go gridSize rowSize [] $ reverse arr
              where
                renderRow :: Int -> Array CalendarItem -> H.HTML Void (ChildQuery e)
                renderRow offset items =
                  HH.div
                    [ HP.class_ $ HH.ClassName "flex" ]
                    ( mapWithIndex (\i item -> renderItem (i + offset) item) items )

                gridSize = length arr
                rowSize  = length <<< (unsafePartial fromJust) <<< head $ arr

                go :: Int -> Int -> _ -> Array (Array CalendarItem) -> _
                go 0 _      acc xs = acc
                go n offset acc xs =
                  go
                    (n - 1)
                    offset
                    ((renderRow ((n - 1) * offset) ((unsafePartial fromJust <<< head) xs)) : acc)
                    (drop 1 xs)


            renderItem :: Int -> CalendarItem -> H.HTML Void (ChildQuery e)
            renderItem index item =
              HH.div
                -- Use raw style attribute for convenience.
                ( attachItemProps index item
                [ HP.class_ $ HH.ClassName $ "w3 pa3" <> (if cst.highlightedIndex == Just index then " bg-washed-green" else "")
                , HP.attr (H.AttrName "style") (getCalendarStyles item) ]
                )
                [ HH.text $ printDay item ]
              where
                -- If the calendar item is selectable, augment the props with the correct click events.
                attachItemProps :: Int -> CalendarItem -> _ -> _
                attachItemProps index (CalendarItem Selectable _ _ _) props = getItemProps index props
                attachItemProps _ _ props = props

                -- Get the correct styles for a calendar item dependent on its statuses
                getCalendarStyles :: CalendarItem -> String
                getCalendarStyles i
                  =  getSelectableStyles i
                  <> " " <> getSelectedStyles i
                  <> " " <> getBoundaryStyles i
                  where
                    getSelectableStyles:: CalendarItem -> String
                    getSelectableStyles (CalendarItem NotSelectable _ _ _)
                      = "color: rgba(0,0,0,0.6); background-image: linear-gradient(to bottom, rgba(125,125,125,0.75) 0%, rgba(125,125,125,0.75), 100%;"
                    getSelectableStyles _ = mempty

                    getSelectedStyles :: CalendarItem -> String
                    getSelectedStyles (CalendarItem _ Selected _ _) = "color: white; background-color: green;"
                    getSelectedStyles _ = mempty

                    getBoundaryStyles :: CalendarItem -> String
                    getBoundaryStyles (CalendarItem _ _ OutOfBounds _) = "opacity: 0.5;"
                    getBoundaryStyles _ = mempty

                printDay :: CalendarItem -> String
                printDay (CalendarItem _ _ _ d) = printDay' d
                  where
                    printDay' :: Date -> String
                    printDay' = (unsafePartial fromRight)
                      <<< formatDateTime "D"
                      <<< toDateTime
                      <<< fromDate


{-

Helpers

-}

-- Generate a standard set of dates from a year and month.
generateCalendarRows :: Year -> Month -> Array CalendarItem
generateCalendarRows y m = lastMonth <> thisMonth <> nextMonth
  where
    { pre, body, post, all } = alignByWeek y m
    outOfBounds = map (\i -> CalendarItem Selectable NotSelected OutOfBounds i)
    lastMonth   = outOfBounds pre
    nextMonth   = outOfBounds post
    thisMonth   = body <#> (\i -> CalendarItem Selectable NotSelected InBounds i)

