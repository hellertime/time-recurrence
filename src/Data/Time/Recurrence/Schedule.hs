{-# LANGUAGE GADTs, StandaloneDeriving #-}
-- This module is intended to be imported @qualified!, to avoid name
-- clashes with "Prelude" functions. eg.
--
-- > import qualified Data.Time.Recurrence.Schedule as S
module Data.Time.Recurrence.Schedule
    (
      Schedule -- abstract, instances: Eq, Ord, Show
    , recur
    , starting
    , begin

    , enumMonths
    , enumDays
    , enumWeekDaysInWeek
    , enumWeekDaysInMonth
    , enumYearDays
    , enumHours
    , enumMinutes
    , enumSeconds

    , nthMonth
    , nthDay
    , nthWeekDay
    , nthWeekDayOfWeek
    , nthWeekDayOfMonth
    , nthYearDay
    , nthHour
    , nthMinute
    , nthSecond

    , filterMonths
    , filterDays
    , filterWeekDays
    , filterYearDays
    , filterHours
    , filterMinutes
    , filterSeconds
    )
  where

import Control.Monad.Reader
import Data.List as L
import Data.List.Ordered as O
import Data.Maybe (mapMaybe)
import Data.Time.Calendar.Month
import Data.Time.Calendar.WeekDay
import Data.Time.CalendarTime hiding (withDay)
import Data.Time.Moment hiding (interval, startOfWeek)
import qualified Data.Time.Moment as M (interval, startOfWeek)

type Schedule a = Reader (InitialMoment a) [a]

runSchedule :: Schedule a -> InitialMoment a -> [a]
runSchedule = runReader

type FutureMoments a = Schedule a

  -> ([a] -> FutureMoments a)
  -> [a]
repeatSchedule im sch = runSchedule (iterateInitialMoment >>= sch) im
  where
    iterateInitialMoment :: Moment a => FutureMoments a
    iterateInitialMoment = do
      im <- ask
      return $ iterate (next (interval im) (period im)) (moment im)

-- iterateMoments :: (Ord a, Moment a) => Schedule' -> a -> [a]
-- iterateMoments = dropWhile (< m0) . O.nub . (uncurry repeatSchedule) . compile

data Freq
    = Secondly { interval :: Interval, startOfWeek :: StartOfWeek }
    | Minutely { interval :: Interval, startOfWeek :: StartOfWeek }
    | Hourly   { interval :: Interval, startOfWeek :: StartOfWeek }
    | Daily    { interval :: Interval, startOfWeek :: StartOfWeek }
    | Weekly   { interval :: Interval, startOfWeek :: StartOfWeek }
    | Monthly  { interval :: Interval, startOfWeek :: StartOfWeek }
    | Yearly   { interval :: Interval, startOfWeek :: StartOfWeek }
  deriving (Show)

defaultFreq :: (Interval -> StartOfWeek -> Freq) -> Freq
defaultFreq = flip uncurry (toInterval 1, toStartOfWeek Sunday)

secondly :: Freq
secondly = defaultFreq Secondly

minutely :: Freq
minutely = defaultFreq Minutely

hourly :: Freq
houtly = defaultFreq Hourly

daily :: Freq
daily = defaultFreq Daily

weekly :: Freq
weekly = defaultFreq Weekly

monthly :: Freq
monthly = defaultFreq Monthly

yearly :: Freq
yearly = defaultFreq Yearly

data Schedule' a where
    Recur :: Freq -> Schedule' Freq
    Then  :: Schedule' Freq -> ScheduleDetails -> Schedule' ScheduleDetails

deriving instance Show (Schedule' a)

data ScheduleDetails a where
    Enumerate :: NominalPeriodSelector -> ScheduleDetails NominalPeriodSelector
    Filter    :: NominalPeriodFilter -> ScheduleDetails NominalPeriodFilter
    Select    :: OrdinalPeriodSelector -> ScheduleDetails OrdinalPeriodSelector
    :&        :: ScheduleDetails NominalPeriodSelector -> ScheduleDetails NominalPeriodSelector -> ScheduleDetails NominalPeriodSelector
    :|        :: ScheduleDetails NominalPeriodFilter -> ScheduleDetails NominalPeriodFilter -> ScheduleDetails NominalPeriodFilter
    :!!       :: ScheduleDetails OrdinalPeriodSelector -> ScheduleDetails OrdinalPeriodSelector -> ScheduleDetails OrdinalPeriodFilter
    :>        :: ScheduleDetails NominalPeriodSelector -> ScheduleDetails NominalPeriodFilter -> ScheduleDetails NominalPeriodFilter
    :>>       :: ScheduleDetails NominalPeriodFilter -> ScheduleDetails OrdinalPeriodSelector -> ScheduleDetails OrdinalPeriodSelector
    :>>>      :: ScheduleDetails NominalPeriodSelector -> ScheduleDetails OrdinalPeriodSelector -> ScheduleDetails OrdinalPeriodSelector

deriving instance Show (ScheduleDetails a)

compile :: (Ord a, Moment a) => Schedule' -> a -> ([a] -> FutureMoments a)
compile (Then recur details m0) = starting (mkIM recur) m0 $ compile' details
compile recur                   = begin (mkIM recur) m0 return
  where
    mkIM' :: Moment a => Period -> Interval -> StartOfWeek -> InitialMoment a
    mkIM' per int sow = InitialMoment per int sow epoch
    mkIM :: Moment a => (Freq -> Schedule' Freq) -> InitialMoment a
    mkIM (Recur freq int sow) =
        mkIM' (case freq of Secondly -> Seconds
                            Minutely -> Minutes
                            Hourly   -> Hours
                            Daily    -> Days
                            Weekly   -> Weeks
                            Monthly  -> Months
                            Yearly   -> Years) int sow

recur :: a -> a
recur = id

-- occurrs before the 'InitialMoment'. The list is further refined
-- by the passed in function.
  -> [a]
starting im m0 = dropWhile (< m0) . O.nub . repeatSchedule im{moment=m0}

begin :: (Ord a, Moment a) => InitialMoment a -> a -> [a]
begin im m0 = starting im m0 return

-- | Normalize an bounded ordinal index
--   Pass an upper-bound 'ub' and an index 'idx'
--   Converts 'idx' < 0 into valid 'idx' > 0 or
--   Nothing
normalizeOrdinalIndex :: Int -> Int -> Maybe Int
normalizeOrdinalIndex _ 0 = Nothing
normalizeOrdinalIndex ub idx =
  if abs idx > ub
    then Nothing
    else Just $ (idx + ub') `mod` ub'
  where
    ub' = ub + 1

enumYearDays ::
  (CalendarTimeConvertible a, Moment a) =>
  [Int]
  -> [a]
  -> FutureMoments a
enumYearDays days as = return $ concatMap (enumYearDays' days) as
  where
    enumYearDays' days a = mapMaybe (withYearDay a) (days' a days)
    days' a = mapMaybe $ normalizeOrdinalIndex (daysInYear a)

  -> FutureMoments a
enumMonths months as = return $ concatMap (enumMonths' months) as
    enumMonths' months a = mapMaybe (withMonth a) months

enumDays ::
  (CalendarTimeConvertible a, Moment a) =>
  [Int]
  -> [a]
  -> FutureMoments a
enumDays days as = return $ concatMap (enumDays' days) as
    enumDays' days a = mapMaybe (withDay a) (days' a days)
    days' a = mapMaybe $ normalizeOrdinalIndex (lastDayOfMonth a)

enumWeekDaysInWeek ::
  (CalendarTimeConvertible a, Moment a) =>
  [WeekDay]
  -> [a]
  -> FutureMoments a
enumWeekDaysInWeek wdays as = return $ concatMap (enumWeekDays' wdays) as
  where
    enumWeekDays' :: (CalendarTimeConvertible a, Moment a) => [WeekDay] -> a -> [a]
    enumWeekDays' wdays a0 = let
      w0     = calendarWeekDay $ toCalendarTime a0
      wdays' = dropWhile (/= w0) $ O.nubSort wdays
      in map (advanceToWeekDay a0) wdays'

enumWeekDaysInMonth ::
  (CalendarTimeConvertible a, Moment a) =>
  [WeekDay]
  -> [a]
  -> FutureMoments a
enumWeekDaysInMonth wdays as = return $ concatMap (enumWeekDays' wdays) as
  where
    enumWeekDays' wdays a = let
      mdays  = mapMaybe (withDay a) [1 .. lastDayOfMonth a]
      in filter (flip elem wdays . calendarWeekDay . toCalendarTime) mdays
enumHours ::
  (CalendarTimeConvertible a, Moment a) =>
  [Int]
  -> [a]
  -> FutureMoments a
enumHours hours as = return $ concatMap (enumHours' hours) as
  where
    enumHours' hours a = mapMaybe (withHour a) (hours' a hours)
    hours' _ = mapMaybe $ normalizeOrdinalIndex 23

enumMinutes ::
  (CalendarTimeConvertible a, Moment a) =>
  [Int]
  -> [a]
  -> FutureMoments a
enumMinutes ms as = return $ concatMap (enumMinutes' ms) as
  where
    enumMinutes' ms a = mapMaybe (withMinute a) (ms' a ms)
    ms' _ = mapMaybe $ normalizeOrdinalIndex 59

enumSeconds ::
  [Int]
  -> [a]
  -> FutureMoments a
enumSeconds secs as = return $ concatMap (enumSeconds' secs) as
  where
    enumSeconds' secs a = mapMaybe (withSecond a) (secs' a secs)
    secs' _ = mapMaybe $ normalizeOrdinalIndex 60

groupWith :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupWith f = groupBy (\a b -> f a == f b)

nth :: [Int] -> [a] -> [a]
nth ns as = map ((as !!) . pred) $ mapMaybe (normalizeOrdinalIndex (length as)) ns

nth' ::
  (a -> b)
  -> [Int]
  -> [a]
  -> FutureMoments a
nth' f ns as = return $ concatMap (nth ns) $ groupWith f as

nthYearDay ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> FutureMoments a
nthYearDay = nth' $ calendarYear . toCalendarTime

nthMonth ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> FutureMoments a
nthMonth = nth' $ calendarYear . toCalendarTime

nthDay ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> FutureMoments a
nthDay = nth' $ calendarMonth . toCalendarTime

nthWeekDayOfWeek ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> FutureMoments a
nthWeekDayOfWeek ns as = do
  sow <- asks startOfWeek
    concatMap (nth ns) $
    concatMap (groupWith (weekNumber sow)) $

nthWeekDayOfMonth ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> FutureMoments a
nthWeekDayOfMonth = nth' $ calendarMonth . toCalendarTime

nthWeekDay ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> FutureMoments a
nthWeekDay = nth' $ calendarYear . toCalendarTime

nthHour ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> FutureMoments a
nthHour = nth' $ calendarDay . toCalendarTime

nthMinute ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> FutureMoments a
nthMinute = nth' $ calendarHour . toCalendarTime

nthSecond ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> FutureMoments a
nthSecond = nth' $ calendarMinute . toCalendarTime

filterCalendarTime ::
  (CalendarTimeConvertible a, Eq b) =>
  (CalendarTime -> b)
  -> [b]
  -> [a]
  -> FutureMoments a
filterCalendarTime f xs as = return $ filter (flip elem xs . f . toCalendarTime) as

filterMonths ::
  CalendarTimeConvertible a =>
  [Month]
  -> [a]
  -> FutureMoments a
filterMonths = filterCalendarTime calendarMonth

filterYearDays ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> FutureMoments a
filterYearDays = filterCalendarTime calendarYearDay

filterDays ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> FutureMoments a
filterDays = filterCalendarTime calendarDay

filterWeekDays ::
  [WeekDay]
  -> [a]
  -> FutureMoments a
filterWeekDays = filterCalendarTime calendarWeekDay

filterHours ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> FutureMoments a
filterHours = filterCalendarTime calendarHour

filterMinutes ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> FutureMoments a
filterMinutes = filterCalendarTime calendarMinute

filterSeconds ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> FutureMoments a
filterSeconds = filterCalendarTime calendarSecond
