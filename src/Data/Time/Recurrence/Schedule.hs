{-# LANGUAGE GADTs, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, StandaloneDeriving, UndecidableInstances #-}
-- This module is intended to be imported @qualified!, to avoid name
-- clashes with "Prelude" functions. eg.
--
-- > import qualified Data.Time.Recurrence.Schedule as S
module Data.Time.Recurrence.Schedule
    (
      -- * Schedule
      Schedule (..)

      -- * Freq
    , Freq

      -- * Function interface to Recur
    , recur

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
    , monthly
    , yearly

      -- * evaluate a Schedule into a function
    , eval

      -- * run an evaluated Schedule from a moment
    , starting
    )
  where

import Data.List.Ordered as O
import Data.Time.Calendar.Month
import Data.Time.Calendar.WeekDay
import Data.Time.CalendarTime
import Data.Time.Moment hiding (interval, startOfWeek, Period(..))
import qualified Data.Time.Moment as M (Period(..))
import Data.Time.Recurrence.AndThen
import Data.Time.Recurrence.ScheduleDetails hiding (eval)
import qualified Data.Time.Recurrence.ScheduleDetails as D (eval)

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
    And  :: Schedule Freq -> ScheduleDetails b -> Schedule (ScheduleDetails b)

deriving instance Show (Schedule a)

recur :: Freq -> Schedule Freq
recur = Recur

instance AndThen (Schedule Freq) (ScheduleDetails b) (Schedule (ScheduleDetails b)) where
  (>==>) x y = And x y

eval :: (CalendarTimeConvertible a, Ord a, Moment a) => Schedule b -> (a -> [a])
eval (And recur details) = flip (startWith $ mkIM recur) $ D.eval details
eval recur@(Recur _)      = start $ mkIM recur

starting :: (CalendarTimeConvertible a, Ord a, Moment a) => a -> Schedule b -> [a]
starting m0 sch = (eval sch) m0

mkIM :: Moment a => Schedule Freq -> InitialMoment a
mkIM (Recur freq) =
    mkIM' (case freq of (Secondly _ _) -> M.Seconds
                        (Minutely _ _) -> M.Minutes
                        (Hourly   _ _) -> M.Hours
                        (Daily    _ _) -> M.Days
                        (Weekly   _ _) -> M.Weeks
                        (Monthly  _ _) -> M.Months
                        (Yearly   _ _) -> M.Years) (interval freq) (startOfWeek freq)
  where
    mkIM' :: Moment a => M.Period -> Interval -> StartOfWeek -> InitialMoment a
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
