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
import Data.Maybe (mapMaybe, fromJust)
import Data.Time.Calendar.Month
import Data.Time.Calendar.WeekDay
import Data.Time.CalendarTime hiding (withDay)
import Data.Time.Moment

type Schedule a = Reader (InitialMoment a) [a]

runSchedule :: Schedule a -> InitialMoment a -> [a]
runSchedule = runReader

repeatSchedule :: 
  Moment a  => 
  InitialMoment a 
  -> ([a] -> Schedule a) 
  -> [a]
repeatSchedule im sch = runSchedule (iterateInitialMoment >>= sch) im
  where
    iterateInitialMoment :: Moment a => Schedule a
    iterateInitialMoment = do
      im <- ask
      return $ iterate (next (interval im) (period im)) (moment im)

recur :: a -> a
recur = id

-- | 'starting' is an infinite list of 'Moment's, where no 'Moment' 
-- occurrs before the 'InitialMoment'. The list is further refined
-- by the passed in function.
starting :: (Ord a, Moment a) => 
  InitialMoment a 
  -> a 
  -> ([a] -> Schedule a) 
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
  -> Schedule a
enumYearDays days as = return $ concatMap (enumYearDays' days) as
  where
    enumYearDays' days a = mapMaybe (withYearDay a) (days' a days)
    days' a = mapMaybe $ normalizeOrdinalIndex (daysInYear a)

enumMonths :: 
  (CalendarTimeConvertible a, Moment a) => 
  [Month] 
  -> [a] 
  -> Schedule a
enumMonths months as = return $ concatMap (enumMonths' months) as
  where 
    enumMonths' months a = mapMaybe (withMonth a) months

enumDays ::
  (CalendarTimeConvertible a, Moment a) =>
  [Int]
  -> [a]
  -> Schedule a
enumDays days as = return $ concatMap (enumDays' days) as
  where 
    enumDays' days a = mapMaybe (withDay a) (days' a days)
    days' a = mapMaybe $ normalizeOrdinalIndex (lastDayOfMonth a)

enumWeekDaysInWeek ::
  (CalendarTimeConvertible a, Moment a) =>
  [WeekDay]
  -> [a]
  -> Schedule a
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
  -> Schedule a
enumWeekDaysInMonth wdays as = return $ concatMap (enumWeekDays' wdays) as
  where
    enumWeekDays' wdays a = let
      mdays  = mapMaybe (withDay a) [1 .. lastDayOfMonth a]
      in filter (flip elem wdays . calendarWeekDay . toCalendarTime) mdays
      
enumHours ::
  (CalendarTimeConvertible a, Moment a) =>
  [Int]
  -> [a]
  -> Schedule a
enumHours hours as = return $ concatMap (enumHours' hours) as
  where
    enumHours' hours a = mapMaybe (withHour a) (hours' a hours)
    hours' a = mapMaybe $ normalizeOrdinalIndex 23

enumMinutes ::
  (CalendarTimeConvertible a, Moment a) =>
  [Int]
  -> [a]
  -> Schedule a
enumMinutes ms as = return $ concatMap (enumMinutes' ms) as
  where
    enumMinutes' ms a = mapMaybe (withMinute a) (ms' a ms)
    ms' a = mapMaybe $ normalizeOrdinalIndex 59

enumSeconds ::
  (CalendarTimeConvertible a, Moment a) => 
  [Int]
  -> [a]
  -> Schedule a
enumSeconds secs as = return $ concatMap (enumSeconds' secs) as
  where
    enumSeconds' secs a = mapMaybe (withSecond a) (secs' a secs)
    secs' a = mapMaybe $ normalizeOrdinalIndex 60

groupWith :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupWith f = groupBy (\a b -> f a == f b)

nth :: [Int] -> [a] -> [a]
nth ns as = map ((as !!) . pred) $ mapMaybe (normalizeOrdinalIndex (length as)) ns

nth' ::
  Ord b => 
  (a -> b)
  -> [Int]
  -> [a]
  -> Schedule a
nth' f ns as = return $ concatMap (nth ns) $ groupWith f as

nthYearDay ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> Schedule a
nthYearDay = nth' $ calendarYear . toCalendarTime

nthMonth ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> Schedule a
nthMonth = nth' $ calendarYear . toCalendarTime

nthDay ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> Schedule a
nthDay = nth' $ calendarMonth . toCalendarTime

nthWeekDayOfWeek ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> Schedule a
nthWeekDayOfWeek ns as = do
  sow <- asks startOfWeek
  return $ 
    concatMap (nth ns) $
    concatMap (groupWith (weekNumber sow)) $
    groupWith (calendarMonth . toCalendarTime) as 

nthWeekDayOfMonth ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> Schedule a
nthWeekDayOfMonth = nth' $ calendarMonth . toCalendarTime

nthHour ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> Schedule a
nthHour = nth' $ calendarDay . toCalendarTime

nthMinute ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> Schedule a
nthMinute = nth' $ calendarHour . toCalendarTime

nthSecond ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> Schedule a
nthSecond = nth' $ calendarMinute . toCalendarTime

filterCalendarTime ::
  (CalendarTimeConvertible a, Eq b) =>
  (CalendarTime -> b)
  -> [b]
  -> [a]
  -> Schedule a
filterCalendarTime f xs as = return $ filter (flip elem xs . f . toCalendarTime) as

filterMonths ::
  CalendarTimeConvertible a =>
  [Month]
  -> [a]
  -> Schedule a
filterMonths = filterCalendarTime calendarMonth

filterYearDays ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> Schedule a
filterYearDays = filterCalendarTime calendarYearDay

filterDays ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> Schedule a
filterDays = filterCalendarTime calendarDay

filterWeekDays ::
  CalendarTimeConvertible a => 
  [WeekDay]
  -> [a]
  -> Schedule a
filterWeekDays = filterCalendarTime calendarWeekDay

filterHours ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> Schedule a
filterHours = filterCalendarTime calendarHour

filterMinutes ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> Schedule a
filterMinutes = filterCalendarTime calendarMinute

filterSeconds ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> Schedule a
filterSeconds = filterCalendarTime calendarSecond
