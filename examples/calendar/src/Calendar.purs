module Calendar where

import Prelude

import CSS as CSS
import Control.Monad.Aff.Console (log)

import Data.Array (drop, head, last, mapWithIndex, reverse, take, (:))
import Data.Date (Date, Day, Month, Weekday(..), Year, canonicalDate, day, lastDayOfMonth, month, weekday, year)
import Data.DateTime (adjust, date)
import Data.DateTime.Instant (fromDate, toDateTime)
import Data.Either (fromRight)
import Data.Enum (downFrom, toEnum, upFrom)
import Data.Foldable (foldr)
import Data.Formatter.DateTime (formatDateTime)
import Data.Int (toNumber)
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
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
import Select.Dispatch (Dispatch, Item(..), emit, getItemProps)
import Select.Effects (FX)
import Select.Primitive.Container as C


{-

The calendar component is an example.

-}

data Query a
  = HandleContainer (C.Message CalendarItem Query) a

type ChildQuery = Dispatch CalendarItem Query

type State =
  { items    :: Array (Item CalendarItem)
  , selected :: Maybe Selection }

data Selection
  = Single Date
  | Range Date Date

type CalendarItem = Date

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
    initState = { items: jan2018, selected: Nothing }

    render :: State -> H.ParentHTML Query ChildQuery Unit (FX e)
    render st =
      HH.div
        [ HP.class_ $ HH.ClassName "mw8 sans-serif center" ]
        [ HH.h2
          [ HP.class_ $ HH.ClassName "black-80 f-headline-1" ]
          [ HH.text "Calendar Component"]
        , HH.slot unit (C.component renderContainer) { items: jan2018 } (HE.input HandleContainer)
        ]

    eval :: Query ~> H.ParentDSL State Query ChildQuery Unit Void (FX e)
    eval = case _ of
      HandleContainer m a -> case m of
        C.Emit q -> emit eval q a

        -- The only other message raised by the container primitive is when an item has been
        -- selected.
        C.ItemSelected item -> a <$ do
          H.liftAff $ log "Selected!"
          H.liftAff $ log ("Selected! Choice was " <> show item)


{-

Render Functions

-}

-- The user is using the Container primitive, so they have to fill out a Container render function
renderContainer :: (C.State CalendarItem) -> H.HTML Void ChildQuery
renderContainer st =
  HH.div
    [ HP.class_ $ HH.ClassName "dt dt--fixed"
    , HC.style $ CSS.width (CSS.px 320.0) ]
    (renderRow <$> rows)
    where
      rows :: Array (Array (Item CalendarItem))
      rows = reverse $ mkRowsFromRange st.items []


renderRow :: Array (Item CalendarItem) -> H.HTML Void ChildQuery
renderRow items =
  HH.div
    [ HP.class_ $ HH.ClassName "dt-row" ]
    (renderItem `mapWithIndex` items)


renderItem :: Int -> Item CalendarItem -> H.HTML Void ChildQuery
renderItem index (Selected i) =
  HH.div
    [ HP.class_ $ HH.ClassName "dtc pa4 black-70"
    , HC.style $ do
        CSS.width (CSS.px 40.0)
        CSS.height (CSS.px 40.0) ]
    [ HH.text $ printDay i ]
renderItem index (Selectable i) =
  HH.div
    ( getItemProps index [ HP.class_ $ HH.ClassName "dtc pa4 black-70"
    , HC.style $ do
        CSS.width (CSS.px 40.0)
        CSS.height (CSS.px 40.0) ] )
    [ HH.text $ printDay i ]
renderItem index (Disabled _) =
  HH.div
    [ HP.class_ $ HH.ClassName "dtc pa4 black-30 b--moon-gray"
    , HC.style $ do
        CSS.width (CSS.px 40.0)
        CSS.height (CSS.px 40.0) ]
    [ HH.text "" ]

printDay :: Date -> String
printDay = (unsafePartial fromRight)
  <<< formatDateTime "D"
  <<< toDateTime
  <<< fromDate


{-

CONFIGURATION

-}

-- Represents a month with offsets
type RenderableDates =
  { pre  :: Array Date
  , body :: Array Date
  , post :: Array Date
  , all  :: Array Date }

jan2018 :: Array (Item CalendarItem)
jan2018 = map (\d -> Selectable d)
    $ _.all
    $ renderableDates
    (unsafePartial fromJust $ toEnum 2018)
    (unsafePartial fromJust $ toEnum 1)

mkRowsFromRange :: forall a. Array a -> Array (Array a) -> Array (Array a)
mkRowsFromRange [] acc = acc
mkRowsFromRange inp acc = mkRowsFromRange (drop 7 inp) (take 7 inp : acc)

-- Given a month, provide the dates visible to the calendar for
-- that month. The calendar will backfill dates from the previous
-- month if the month does not begin on Sunday, and will pad right
-- if the month does not end on Saturday.
renderableDates :: Year -> Month -> RenderableDates
renderableDates year month = { pre: left, body: range, post: right, all: left <> range <> right }
  where
    start = canonicalDate year month $ unsafePartial fromJust $ toEnum 1
    end    = canonicalDate year month $ lastDayOfMonth year month
    range  = monthRange start
    offset = offsets (weekdays start)
    left   = extendDate start (fst offset)
    right  = extendDate start (snd offset)

-- Make a map with weekdays as keys and ints as values
-- To list out dates in a grid, list out days for each weekday
-- Then, pad the arrays so they are the same length.
-- Find the first location of '1' and the weekday for it
-- All previous weekdays get blanks filled in
-- Find the location of the last number and the weekday for it
-- All following weekdays get blanks filled in.

-- tes
-- a= monthRange $ unsafePartial $ fromJust $ mkDate 2018 1 1
offsets :: Array Weekday -> Tuple Int Int
offsets xs = Tuple left right
  where
    left  = offsetL $ fromMaybe Sunday $ head xs
    right = offsetR $ fromMaybe Saturday $ last xs

weekdays :: Date -> Array Weekday
weekdays d = weekday <$> monthRange d

offsetL :: Weekday -> Int
offsetL Sunday = 0
offsetL Monday = 0 - 1
offsetL Tuesday = 0 - 2
offsetL Wednesday = 0 - 3
offsetL Thursday = 0 - 4
offsetL Friday = 0 - 5
offsetL Saturday = 0 - 6

offsetR :: Weekday -> Int
offsetR Sunday = 6
offsetR Monday = 5
offsetR Tuesday = 4
offsetR Wednesday = 3
offsetR Thursday = 2
offsetR Friday = 1
offsetR Saturday = 0

-- Produces a range of each date in the month.
monthRange :: Date -> Array Date
monthRange d =
  let days = day d : upFrom (day d)
      m = month d
      y = year d
  in (\d -> canonicalDate y m d) <$> days

rangeFromDays :: Date -> Array Day -> Array Date
rangeFromDays d days =
  let m = month d
      y = year d
  in (\day -> canonicalDate y m day) <$> days


extendDate :: Date -> Int -> Array Date
extendDate start n =
  let offset = date
               $ unsafePartial fromJust
               $ adjust (Days $ toNumber n)
               $ toDateTime <<< fromDate
               $ start

      isPositive = n >= 0
      up d   = take n $ day d : upFrom (day d)
      down d = take (abs n) $ reverse $ (downFrom $ day d) <> [ day d ]

  in if isPositive
     then rangeFromDays start (up start)
     else rangeFromDays start (down offset)


mkDate :: Int -> Int -> Int -> Maybe Date
mkDate y m d = do
  year  <- toEnum y
  month <- toEnum m
  day   <- toEnum d
  pure  $ canonicalDate year month day
