module Data.Time.CalendarTime.CalendarTime
    (
      -- * Calendar Time
      CalendarTime (..)
    , toDay
    , withDay
    , toTimeOfDay
    , daysInYear
    , lastDayOfMonth
    , weekNumber

      -- * Calendar Time Convertible
    , CalendarTimeConvertible (..)
    ) 
  where

import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.Month
import Data.Time.Calendar.WeekDay
import Data.Time.Moment.StartOfWeek
import System.IO.Unsafe

-- | A representation of calendar time separated into year, month, day, and so on.
data CalendarTime = CalendarTime 
    {
      calendarSecond   :: Int
    , calendarMinute   :: Int
    , calendarHour     :: Int
    , calendarDay      :: Int
    , calendarMonth    :: Month
    , calendarYear     :: Integer
    , calendarWeekDay  :: WeekDay
    , calendarYearDay  :: Int
    , calendarTimeZone :: TimeZone
} deriving (Eq,Ord,Show)

-- | The class of types which can be converted to a 'CalendarTime'
class CalendarTimeConvertible t where
  -- | Convert to a 'CalendarTime'
  toCalendarTime :: t -> CalendarTime
  -- | Convert from a 'CalendarTime'
  fromCalendarTime :: CalendarTime -> Maybe t

instance CalendarTimeConvertible CalendarTime where
  toCalendarTime = id
  fromCalendarTime = Just . id

-- | Convert to a 'Day'
toDay :: CalendarTime -> Maybe Day
toDay t = fromGregorianValid (calendarYear t) (fromEnum $ calendarMonth t) (calendarDay t)

-- | Convert to a 'TimeOfDay'
toTimeOfDay :: CalendarTime -> Maybe TimeOfDay
toTimeOfDay t = makeTimeOfDayValid (calendarHour t) (calendarMinute t) (toEnum $ calendarSecond t)

-- | Change y-m-d in 'CalendarTime'
withDay :: CalendarTime -> Day -> CalendarTime
withDay ct day = ct
               { calendarYear = y
               , calendarMonth = toEnum m
               , calendarDay = d}
  where
    (y, m, d) = toGregorian day

dayInfo :: 
  Day 
  -> ( Integer -- Year
     , Int     -- Month
     , Int     -- Day
     , WeekDay -- Week Day
     , Int     -- Year Day
     )
dayInfo day = let
  (y, m, d) = toGregorian day
  weekDay   = toEnum $ snd (mondayStartWeek day) - 1
  yearDay   = snd $ toOrdinalDate day
  in (y, m, d, weekDay, yearDay)
  

instance CalendarTimeConvertible UTCTime where
  toCalendarTime (UTCTime utcDay utcTime) = CalendarTime (fromEnum ss) mm hh d (toEnum m) y weekDay yearDay utc
    where
      (TimeOfDay hh mm ss) = timeToTimeOfDay utcTime
      (y, m, d, weekDay, yearDay) = dayInfo utcDay

  fromCalendarTime t = do
    day <- toDay t
    time <- toTimeOfDay t
    return $ UTCTime day (timeOfDayToTime time)

instance CalendarTimeConvertible LocalTime where
  toCalendarTime (LocalTime day t) = CalendarTime (fromEnum $ todSec t) (todMin t) (todHour t) d (toEnum m) y weekDay yearDay tz
    where
      (y, m, d, weekDay, yearDay) = dayInfo day
      tz = unsafePerformIO getCurrentTimeZone

  fromCalendarTime t = do
    day <- toDay t
    time <- toTimeOfDay t
    return $ LocalTime day time

instance CalendarTimeConvertible ZonedTime where
  toCalendarTime (ZonedTime (LocalTime day t) tz) = CalendarTime (fromEnum $ todSec t) (todMin t) (todHour t) d (toEnum m) y weekDay yearDay tz
    where
      (y, m, d, weekDay, yearDay) = dayInfo day

  fromCalendarTime t = do
    day <- toDay t
    time <- toTimeOfDay t
    return $ ZonedTime (LocalTime day time) (calendarTimeZone t)

daysInYear :: (CalendarTimeConvertible a) => a -> Int
daysInYear t = let ct = toCalendarTime t
  in if isLeapYear $ calendarYear ct then 366 else 365

lastDayOfMonth :: (CalendarTimeConvertible a) => a -> Int
lastDayOfMonth t = let ct = toCalendarTime t
  in gregorianMonthLength (calendarYear ct) (fromEnum $ calendarMonth ct)

weekNumber :: (CalendarTimeConvertible a) => StartOfWeek -> a -> Maybe Int
weekNumber _  t = do
  day <- toDay $ toCalendarTime t
  return $ fst $ mondayStartWeek day
