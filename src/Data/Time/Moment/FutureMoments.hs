module Data.Time.Moment.FutureMoments
    (
      FutureMoments -- abstract, instances: Eq, Ord, Show

      -- * Drive the Moment computation
    , iterateFutureMoments

    , enumMonths
    , enumWeeks
    , enumDays
    , enumWeekDaysInWeek
    , enumWeekDaysInMonth
    , enumYearDays
    , enumHours
    , enumMinutes
    , enumSeconds

    , nthMonth
    , nthDay
    , nthWeek
    , nthWeekDay
    , nthWeekDayOfWeek
    , nthWeekDayOfMonth
    , nthYearDay
    , nthHour
    , nthMinute
    , nthSecond

    , filterMonths
    , filterWeeks
    , filterDays
    , filterWeekDays
    , filterYearDays
    , filterHours
    , filterMinutes
    , filterSeconds

    )
  where

import Control.Monad.Reader
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List
import Data.List.Ordered as O
import Data.Time.Calendar.Month
import Data.Time.Calendar.WeekDay
import Data.Time.CalendarTime hiding (withDay)
import Data.Time.Moment.Moment

type FutureMoments a = Reader (InitialMoment a) [a]

iterateFutureMoments :: 
  Moment a  => 
  InitialMoment a
  -> ([a] -> FutureMoments a)
  -> [a]
iterateFutureMoments im sch = runReader (iterateInitialMoment >>= sch) im
  where
    iterateInitialMoment :: Moment a => FutureMoments a
    iterateInitialMoment = do
      im <- ask
      return $ iterate (next (interval im) (period im)) (moment im)

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

enumMonths :: 
  (CalendarTimeConvertible a, Moment a) => 
  [Month] 
  -> [a] 
  -> FutureMoments a
enumMonths months as = return $ concatMap (enumMonths' months) as
  where
    enumMonths' months a = mapMaybe (withMonth a) months

enumWeeks ::
  (CalendarTimeConvertible a, Moment a) =>
  [Int]
  -> [a]
  -> FutureMoments a
enumWeeks weeks as = do
  sow <- asks startOfWeek
  return $ concatMap (enumWeeks' sow weeks) as
 where
   enumWeeks' sow weeks a = mapMaybe (withWeekNumber sow a) weeks

enumDays ::
  (CalendarTimeConvertible a, Moment a) =>
  [Int]
  -> [a]
  -> FutureMoments a
enumDays days as = return $ concatMap (enumDays' days) as
  where
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
  (CalendarTimeConvertible a, Moment a) =>
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
  (Ord b) =>
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

nthWeek ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> FutureMoments a
nthWeek ns as = do
  sow <- asks startOfWeek
  return $
    concatMap (nth ns) $
    groupWith (weekNumber sow . toCalendarTime) as

nthWeekDayOfWeek ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> FutureMoments a
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

filterCalendarTime' ::
  (CalendarTimeConvertible a, Eq b) =>
  (CalendarTime -> b)
  -> [b]
  -> [a]
  -> [a]
filterCalendarTime' f xs as = filter (flip elem xs . f . toCalendarTime) as

filterCalendarTime ::
  (CalendarTimeConvertible a, Eq b) =>
  (CalendarTime -> b)
  -> [b]
  -> [a]
  -> FutureMoments a
filterCalendarTime f xs as = return $ filterCalendarTime' f xs as

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

filterWeeks ::
  CalendarTimeConvertible a =>
  [Int]
  -> [a]
  -> FutureMoments a
filterWeeks wks as = do
  sow <- asks startOfWeek
  return $ filterCalendarTime' (fromMaybe 0 . weekNumber sow) (filter (>0) wks) as

filterWeekDays ::
  CalendarTimeConvertible a =>
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
