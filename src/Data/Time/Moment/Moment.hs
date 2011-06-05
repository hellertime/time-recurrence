module Data.Time.Moment.Moment
    (
      -- * Moment
      Moment (..)
    , iterateMoments
    , withYearDay
    , withWeekNumber
    , withSecond
    , withMinute
    , withHour
    , withDay
    , withMonth
    , withYear
    , advanceToWeekDay

      -- * Initial Moment
    , InitialMoment (..)

      -- * Default Initial Moment
    , secondly
    , minutely
    , hourly
    , daily
    , weekly
    , monthly
    , yearly

      -- * Adjust Interval
    , by
      -- * Adjust Start of Week
    , withStartOfWeek

      -- * Period
    , Period (..)

    , Interval (fromInterval)
    , toInterval

    , StartOfWeek (fromStartOfWeek)
    , toStartOfWeek
    )
  where

import Data.Time.Calendar.Month
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDay
import Data.Time.Calendar.WeekDate
import Data.Time.CalendarTime hiding (withDay)
import qualified Data.Time.CalendarTime as CT

oneSecond :: Integer
oneSecond = 1

oneMinute :: Integer
oneMinute = 60 * oneSecond

oneHour :: Integer
oneHour   = 60 * oneMinute

oneDay :: Integer
oneDay    = 24 * oneHour

oneWeek :: Integer
oneWeek   = 7  * oneDay

-- | The @Moment@ class is for representing a instance in time.
--
-- Instances of @Moment@ can be derived for any user-defined
-- datatype for which can satisfy the minimal complete definition.
--
-- Minimal complete definition: 'epoch', 'addSeconds', 'addMonths', 'addYears'

class Moment a where
  -- | Provide a default moment.
  epoch      :: a
  addSeconds :: a -> Integer -> a
  addMonths  :: a -> Integer -> a
  addYears   :: a -> Integer -> a

  addMinutes :: a -> Integer -> a
  addMinutes a = addSeconds a . (* oneMinute)
  addHours   :: a -> Integer -> a
  addHours a = addSeconds a . (* oneHour)
  addDays    :: a -> Integer -> a
  addDays a = addSeconds a . (* oneDay)
  addWeeks   :: a -> Integer -> a
  addWeeks a = addSeconds a . (* oneWeek)

  -- | Produce a new @Moment@ in the future ocurring at (/interval/ * /freq/)
  next :: Interval -> Period -> a -> a
  next (Interval interval) freq =
    case freq of
      Seconds -> add oneSecond
      Minutes -> add oneMinute
      Hours   -> add oneHour
      Days    -> add oneDay
      Weeks   -> add oneWeek
      Months  -> flip addMonths interval
      Years   -> flip addYears interval
    where
      add x = flip addSeconds (interval * x)

  -- | Produce a new @Moment@ in the past ocurring at (-/interval/ * /freq/)
  prev :: Interval -> Period -> a -> a
  prev (Interval interval) = next $ Interval (-interval)

-- | Produce an infinite list from an initial @Moment@ and a step function.
iterateMoments :: Moment a => (a -> a) -> a -> [a]
iterateMoments = iterate

-- | Possibly produce a 'Moment' with the given week number
withWeekNumber :: 
  (CalendarTimeConvertible a, Moment a) => 
  StartOfWeek 
  -> a 
  -> Int 
  -> Maybe a
withWeekNumber _ t wk = do
  let ct = toCalendarTime t
  day <- fromMondayStartWeekValid (calendarYear ct) wk (fromEnum $ calendarWeekDay ct)
  fromCalendarTime $ CT.withDay ct day

-- | Possibly produce a 'Moment' with the given day of the year
withYearDay ::
  (CalendarTimeConvertible a, Moment a) =>
  a
  -> Int
  -> Maybe a
withYearDay t yd = do
 let ct = toCalendarTime t
 day <- fromOrdinalDateValid (calendarYear ct) yd
 fromCalendarTime $ CT.withDay ct day

-- | Possibly produce a 'Moment' with the given second
withSecond :: (CalendarTimeConvertible a, Moment a) => a -> Int -> Maybe a
withSecond t s = fromCalendarTime (toCalendarTime t){calendarSecond = s}

-- | Possibly produce a 'Moment' with the given minute
withMinute :: (CalendarTimeConvertible a, Moment a) => a -> Int -> Maybe a
withMinute t m = fromCalendarTime (toCalendarTime t){calendarMinute = m}

-- | Possibly produce a 'Moment' with the given hour
withHour :: (CalendarTimeConvertible a, Moment a) => a -> Int -> Maybe a
withHour t h = fromCalendarTime (toCalendarTime t){calendarHour = h}

-- | Possibly produce a 'Moment' with the given month day
withDay :: (CalendarTimeConvertible a, Moment a) => a -> Int -> Maybe a
withDay t d = fromCalendarTime (toCalendarTime t){calendarDay = d}

-- | Possibly produce a 'Moment' with the given month
withMonth :: (CalendarTimeConvertible a, Moment a) => a -> Month -> Maybe a
withMonth t m = fromCalendarTime (toCalendarTime t){calendarMonth = m}

-- | Possibly produce a 'Moment' with the given year
withYear :: (CalendarTimeConvertible a, Moment a) => a -> Integer -> Maybe a
withYear t y = fromCalendarTime (toCalendarTime t){calendarYear = y}

advanceToWeekDay :: (CalendarTimeConvertible a, Moment a) => a -> WeekDay -> a
advanceToWeekDay t d = let
  ct = toCalendarTime t
  d0 = calendarWeekDay ct
  in if d0 == d 
    then t 
    else addDays t $ toInteger $ 1 + (fromEnum d - fromEnum Monday) `mod` 6

-- | The @InitialMoment@ datatype

data InitialMoment a = InitialMoment
    { period      :: Period
    , interval    :: Interval
    , startOfWeek :: StartOfWeek
    , moment      :: a
    }
  deriving (Show)

mkIM :: Moment a => Period -> InitialMoment a
mkIM f = InitialMoment f (toInterval 1) (StartOfWeek Monday) epoch

-- | Default initial moments

secondly :: Moment a => InitialMoment a
secondly = mkIM Seconds

minutely :: Moment a => InitialMoment a
minutely = mkIM Minutes

hourly :: Moment a => InitialMoment a
hourly = mkIM Hours

daily :: Moment a => InitialMoment a
daily = mkIM Days

weekly :: Moment a => InitialMoment a
weekly = mkIM Weeks

monthly :: Moment a => InitialMoment a
monthly = mkIM Months

yearly :: Moment a => InitialMoment a
yearly = mkIM Years

-- | Typically called infix on an existing 'InitialMoment', like:
-- 
-- > monthly `by` 2
by :: InitialMoment a -> Integer -> InitialMoment a
by im i = im{interval=toInterval i}

-- | Typically called infix on an existing 'InitialMoment', like:
--
-- > weekly `withStartOfWeek` Tuesday
withStartOfWeek :: InitialMoment a -> WeekDay -> InitialMoment a
withStartOfWeek im sow = im{startOfWeek=toStartOfWeek sow}

-- | @Period@ data type
data Period
    = Seconds
    | Minutes
    | Hours
    | Days
    | Weeks
    | Months
    | Years
  deriving (Enum, Bounded, Eq, Ord, Show)

newtype Interval = Interval { fromInterval :: Integer } deriving (Show)

toInterval :: Integer -> Interval
toInterval = Interval

newtype StartOfWeek = StartOfWeek { fromStartOfWeek :: WeekDay } deriving (Show)

toStartOfWeek :: WeekDay -> StartOfWeek
toStartOfWeek = StartOfWeek

