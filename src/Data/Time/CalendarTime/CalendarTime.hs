module Data.Time.CalendarTime.CalendarTime
    (
      -- * Calendar Time
      CalendarTime(..)
    , toDay
    , toTimeOfDay
    ) 
  where

import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.Gregorian
import Data.Time.Calendar.Days
import Data.Time.Calendar.Month
import Data.Time.Calendar.WeekDay
import Data.Time.Calendar.Private
import Data.Time.Clock
import Data.Time.LocalTime.LocalTime
import Data.Time.LocalTime.TimeOfDay
import Data.Time.LocalTime.TimeZone
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
} deriving (Eq,Ord)

instance Show CalendarTime where
  show = show . fromCalendarTime

-- | The class of types which can be converted to a 'CalendarTime'
class CalendarTimeConvertible t where
  -- | Convert to a 'CalendarTime'
  toCalendarTime :: t -> CalendarTime
  -- | Convert from a 'CalendarTime'
  fromCalendarTime :: CalendarTime -> Maybe t

-- | Convert to a 'Day'
toDay :: CalendarTime -> Maybe Day
toDay t = fromGregorianValid (calendarYear t) (fromEnum $ calendarMonth t) (calendarDay t)

-- | Convert to a 'TimeOfDay'
toTimeOfDay :: CalendarTime -> Maybe TimeOfDay
toTimeOfDay t = makeTimeOfDayValid (calendarHour t) (calendarMinute t) (toEnum $ calendarSecond t)


instance CalendarTimeConvertible UTCTime where
  toCalendarTime (UTCTime utcDay utcTime) = CalendarTime (fromEnum ss) mm hh d (toEnum m) y weekDay yearDay utc
    where
      (TimeOfDay hh mm ss) = timeToTimeOfDay utcTime
      (y, m, d) = toGregorian utcDay
      yearDay = snd $ toOrdinalDate utcDay
      weekDay = toEnum $ snd (mondayStartWeek utcDay) - 1

  fromCalendarTime t = do
    day <- toDay t
    time <- toTimeOfDay t
    return $ UTCTime day (timeOfDayToTime time)

instance CalendarTimeConvertible LocalTime where
  toCalendarTime (LocalTime day t) = CalendarTime (fromEnum $ todSec t) (todMin t) (todHour t) d (toEnum m) y weekDay yearDay tz
    where
      (y, m , d) = toGregorian day
      yearDay = snd $ toOrdinalDate day
      weekDay = toEnum $ snd (mondayStartWeek day) - 1
      tz = unsafePerformIO getCurrentTimeZone

  fromCalendarTime t = do
    day <- toDay t
    time <- toTimeOfDay t
    return $ LocalTime day time

instance CalendarTimeConvertible ZonedTime where
  toCalendarTime (ZonedTime (LocalTime day t) tz) = CalendarTime (fromEnum $ todSec t) (todMin t) (todHour t) d (toEnum m) y weekDay yearDay tz
    where
      (y, m, d) = toGregorian day
      yearDay = snd $ toOrdinalDate day
      weekDay = toEnum $ snd (mondayStartWeek day) - 1

  fromCalendarTime t = do
    day <- toDay t
    time <- toTimeOfDay t
    return $ ZonedTime (LocalTime day time) (calendarTimeZone t)
