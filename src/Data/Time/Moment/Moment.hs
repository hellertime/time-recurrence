module Data.Time.Moment.Moment
    (
      -- * Moment
      Moment (..)
    , iterateMoments
    , withYearDay
    , withWeekDay
    , withSecond
    , withMinute
    , withHour
    , withDay
    , withMonth
    , withYear

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

      -- * Initial Moment with Interval
    , secondlyBy
    , minutelyBy
    , hourlyBy
    , dailyBy
    , weeklyBy
    , monthlyBy
    , yearlyBy

      -- * Initial Moment with Interval and StartOfWeek
    , secondlyWithStartOfWeekBy
    , minutelyWithStartOfWeekBy
    , hourlyWithStartOfWeekBy
    , dailyWithStartOfWeekBy
    , weeklyWithStartOfWeekBy
    , monthlyWithStartOfWeekBy
    , yearlyWithStartOfWeekBy

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
import Data.Time.CalendarTime
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

momentsWithStartOfWeekBy :: 
  Moment a => 
  Period             -- ^ Period of repitition for the generated 'Moment's
  -> Integer         -- ^ Spacing between periods
  -> WeekDay         -- ^ Used when determining what week a Day is a part of
  -> a               -- ^ Initial 'Moment'
  -> InitialMoment a -- ^ 'InitialMoment'
momentsWithStartOfWeekBy p i s m0 = (m `asTypeOf` m0)
                                  { interval    = toInterval i
                                  , startOfWeek = toStartOfWeek s
                                  , moment      = m0
                                  }
  where
    m = case p of
          Seconds -> secondly
          Minutes -> minutely
          Hours   -> hourly
          Days    -> daily
          Weeks   -> weekly
          Months  -> monthly
          Years   -> yearly

momentsBy :: Moment a => Period -> Integer -> a -> InitialMoment a
momentsBy p i m0 = (m `asTypeOf m0)
                 { interval = toInterval i
                 , moment   = m0
                 }
  where
    m = case p of
          Seconds -> secondly
          Minutes -> minutely
          Hours   -> hourly
          Days    -> daily
          Weeks   -> weekly
          Months  -> monthly
          Years   -> yearly

secondlyWithStartOfWeekBy ::
  Moment a =>
  Integer
  -> WeekDay
  -> a
  -> InitialMoment a
secondlyWithStartOfWeekBy = momentsWithStartOfWeekBy Seconds

secondlyBy :: Moment a => Integer -> a -> InitialMoment a
secondlyBy = momentsBy Seconds

minutelyWithStartOfWeekBy ::
  Moment a =>
  Integer
  -> WeekDay
  -> a
  -> InitialMoment a
minutelyWithStartOfWeekBy = momentsWithStartOfWeekBy Minutes

minutelyBy :: Moment a => Integer -> a -> InitialMoment a
minutelyBy i = momentsBy Minutes

hourlyWithStartOfWeekBy ::
  Moment a =>
  Integer
  -> WeekDay
  -> a
  -> InitialMoment a
hourlyWithStartOfWeekBy = momentsWithStartOfWeekBy Hours

hourlyBy :: Moment a => Integer -> a -> InitialMoment a
hourlyBy = momentsBy Hours

dailyWithStartOfWeekBy ::
  Moment a =>
  Integer
  -> WeekDay
  -> a
  -> InitialMoment a
dailyWithStartOfWeekBy = momentsWithStartOfWeekBy Days

dailyBy :: Moment a => Integer -> a -> InitialMoment a
dailyBy = momentsBy Days

weeklyWithStartOfWeekBy ::
  Moment a =>
  Integer
  -> WeekDay
  -> a
  -> InitialMoment a
weeklyWithStartOfWeekBy = momentsWithStartOfWeekBy Days

weeklyBy :: Moment a => Integer -> a -> InitialMoment a
weeklyBy = momentsBy Days

monthlyWithStartOfWeekBy ::
  Moment a => 
  Integer
  -> WeekDay
  -> a
  -> InitialMoment a
monthlyWithStartOfWeekBy = momentsWithStartOfWeekBy Months

monthlyBy :: Moment a => Integer -> a -> InitialMoment a
monthlyBy = momentsBy Months

yearlyWithStartOfWeekBy ::
  Moment a =>
  Integer
  -> WeekDay
  -> a
  -> InitialMoment a
yearlyWithStartOfWeekBy = momentsWithStartOfWeekBy Years

yearlyBy :: Moment a => Integer -> a -> InitialMoment a
yearlyBy = momentsBy Years

-- | @Period@ data type
data Period
    = Seconds
    | Minutes
    | Hours
    | Days
    | Weeks
    | Months
    | Years
  deriving (Show)

newtype Interval = Interval { fromInterval :: Integer } deriving (Show)

toInterval :: Integer -> Interval
toInterval = Interval

newtype StartOfWeek = StartOfWeek { fromStartOfWeek :: WeekDay } deriving (Show)

toStartOfWeek :: WeekDay -> StartOfWeek
toStartOfWeek = StartOfWeek
