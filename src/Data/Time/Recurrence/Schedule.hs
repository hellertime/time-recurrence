{-# LANGUAGE GADTs, StandaloneDeriving #-}
-- This module is intended to be imported @qualified!, to avoid name
-- clashes with "Prelude" functions. eg.
--
-- > import qualified Data.Time.Recurrence.Schedule as S
module Data.Time.Recurrence.Schedule
    (
      -- * Schedule
      Schedule (..)

      -- * Freq
    , Freq (..)

      -- * Adjust Interval
    , by
      -- * Adjust Start of Week
    , withStartOfWeek

      -- * Default Freq
    , secondly
    , minutely
    , hourly
    , daily
    , weekly
    , yearly

      -- * evaluate a Schedule into a function
    , eval

      -- * run an evaluated Schedule from a moment
    , starting
    )
  where

import Control.Monad ((>=>))
import Data.List.Ordered as O
import Data.Time.Calendar.Month
import Data.Time.Calendar.WeekDay
import Data.Time.CalendarTime
import Data.Time.Moment hiding (interval, startOfWeek)
import Data.Time.Recurrence.ScheduleDetails

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
hourly = defaultFreq Hourly

daily :: Freq
daily = defaultFreq Daily

weekly :: Freq
weekly = defaultFreq Weekly

monthly :: Freq
monthly = defaultFreq Monthly

yearly :: Freq
yearly = defaultFreq Yearly

-- | Typically called infix on an existing 'Freq', like:
-- 
-- > monthly `by` 2
by :: Freq -> Integer -> Freq
by fr i = fr{interval=toInterval i}

-- | Typically called infix on an existing 'Freq', like:
--
-- > weekly `withStartOfWeek` Tuesday
withStartOfWeek :: Freq -> WeekDay -> Freq
withStartOfWeek fr sow = fr{startOfWeek=toStartOfWeek sow}


data Schedule a where
    Recur :: Freq -> Schedule Freq
    Then  :: Schedule Freq -> ScheduleDetails b -> Schedule (ScheduleDetails b)

deriving instance Show (Schedule a)

eval' :: (CalendarTimeConvertible a, Ord a, Moment a) => ScheduleDetails b -> ([a] -> FutureMoments a)
eval' (Enumerate x) = case x of
    (EPSeconds ss)         -> enumSeconds ss
    (EPMinutes mm)         -> enumMinutes mm
    (EPHours hh)           -> enumHours hh
    (EPWeekDaysInWeek ww)  -> enumWeekDaysInWeek ww
    (EPWeekDaysInMonth ww) -> enumWeekDaysInMonth ww
    (EPDays dd)            -> enumDays dd
    (EPMonths mm)          -> enumMonths mm
    (EPYearDays yy)        -> enumYearDays yy
eval' (Filter x) = case x of
    (FPSeconds ss)  -> filterSeconds ss
    (FPMinutes mm)  -> filterMinutes mm
    (FPHours hh)    -> filterHours hh
    (FPWeekDays ww) -> filterWeekDays ww
    (FPDays dd)     -> filterDays dd
    (FPMonths mm)   -> filterMonths mm
    (FPYearDays yy) -> filterYearDays yy
eval' (Select x) = case x of
    (SPSeconds ss)         -> nthSecond ss
    (SPMinutes mm)         -> nthMinute mm
    (SPHours hh)           -> nthHour hh
    (SPWeekDaysInWeek ww)  -> nthWeekDayOfWeek ww
    (SPWeekDaysInMonth ww) -> nthWeekDayOfMonth ww
    (SPDays dd)            -> nthDay dd
    (SPMonths mm)          -> nthDay mm
    (SPYearDays yy)        -> nthYearDay yy
eval' ((:&) x y)   = eval' x >=> eval' y
eval' ((:|) x y)   = eval' x >=> eval' y
eval' ((:!!) x y)  = eval' x >=> eval' y
eval' ((:>) x y)   = eval' x >=> eval' y
eval' ((:>>) x y)  = eval' x >=> eval' y
eval' ((:>>>) x y) = eval' x >=> eval' y

eval :: (CalendarTimeConvertible a, Ord a, Moment a) => Schedule b -> (a -> [a])
eval (Then recur details) = flip (startWith $ mkIM recur) $ eval' details
eval recur@(Recur _)      = start $ mkIM recur

starting :: (CalendarTimeConvertible a, Ord a, Moment a) => a -> Schedule b -> [a]
starting m0 sch = (eval sch) m0

mkIM :: Moment a => Schedule Freq -> InitialMoment a
mkIM (Recur freq) =
    mkIM' (case freq of (Secondly _ _) -> Seconds
                        (Minutely _ _) -> Minutes
                        (Hourly   _ _) -> Hours
                        (Daily    _ _) -> Days
                        (Weekly   _ _) -> Weeks
                        (Monthly  _ _) -> Months
                        (Yearly   _ _) -> Years) (interval freq) (startOfWeek freq)
  where
    mkIM' :: Moment a => Period -> Interval -> StartOfWeek -> InitialMoment a
    mkIM' per int sow = InitialMoment per int sow epoch

-- | 'startWith' is an infinite list of 'Moment's, where no 'Moment' 
-- occurrs before the 'InitialMoment'. The list is further refined
-- by the passed in function.
startWith :: (Ord a, Moment a) => 
  InitialMoment a 
  -> a 
  -> ([a] -> FutureMoments a)
  -> [a]
startWith im m0 = dropWhile (< m0) . O.nub . iterateFutureMoments im{moment=m0}

start :: (Ord a, Moment a) => InitialMoment a -> a -> [a]
start im m0 = startWith im m0 return
