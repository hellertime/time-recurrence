{-# LANGUAGE GADTs, StandaloneDeriving #-}
-- This module is intended to be imported @qualified!, to avoid name
-- clashes with "Prelude" functions. eg.
--
-- > import qualified Data.Time.Recurrence.Schedule as S
module Data.Time.Recurrence.Schedule
    (
      -- * Schedule
      Schedule (..)
    , ScheduleDetails (..)

    , EnumSet (..)
    , FilterSet (..)
    , SelectSet (..)

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
    )
  where

import Control.Monad ((>=>))
import Data.List.Ordered as O
import Data.Time.Calendar.Month
import Data.Time.Calendar.WeekDay
import Data.Time.CalendarTime
import Data.Time.Moment hiding (interval, startOfWeek)

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

type Second = Int
type Minute = Int
type Hour = Int
type Day = Int
type YearDay = Int

data EnumSet
    = TheSeconds [Second]
    | TheMinutes [Minute]
    | TheHours   [Hour]
    | TheWeekDaysInWeek [WeekDay]
    | TheWeekDaysInMonth [WeekDay]
    | TheDays [Day]
    | TheMonths [Month]
    | TheYearDays [YearDay]
  deriving (Read, Show)

data FilterSet
    = OnSeconds [Second]
    | OnMinutes [Minute]
    | OnHours [Hour]
    | OnWeekDays [WeekDay]
    | OnDays [Day]
    | OnMonths [Month]
    | OnYearDays [YearDay]
  deriving (Read, Show)

data SelectSet
    = FromSeconds [Int]
    | FromMinutes [Int]
    | FromHours [Int]
    | FromWeekDaysInWeek [Int]
    | FromWeekDaysInMonth [Int]
    | FromDays [Int]
    | FromMonths [Int]
    | FromYearDays [Int]
  deriving (Read, Show)

infixr 5 :&, :|, :!!
infixr 0 :>, :>>, :>>>

data ScheduleDetails a where
    Enumerate :: EnumSet -> ScheduleDetails EnumSet
    Filter    :: FilterSet -> ScheduleDetails FilterSet
    Select    :: SelectSet -> ScheduleDetails SelectSet
    (:&)      :: ScheduleDetails EnumSet -> ScheduleDetails EnumSet -> ScheduleDetails EnumSet
    (:|)      :: ScheduleDetails FilterSet -> ScheduleDetails FilterSet -> ScheduleDetails FilterSet
    (:!!)     :: ScheduleDetails SelectSet -> ScheduleDetails SelectSet -> ScheduleDetails SelectSet
    (:>)      :: ScheduleDetails EnumSet -> ScheduleDetails FilterSet -> ScheduleDetails FilterSet
    (:>>)     :: ScheduleDetails FilterSet -> ScheduleDetails SelectSet -> ScheduleDetails SelectSet
    (:>>>)    :: ScheduleDetails EnumSet -> ScheduleDetails SelectSet -> ScheduleDetails SelectSet

deriving instance Show (ScheduleDetails a)

eval' :: (CalendarTimeConvertible a, Ord a, Moment a) => ScheduleDetails b -> ([a] -> FutureMoments a)
eval' (Enumerate x) = case x of
    (TheSeconds ss)         -> enumSeconds ss
    (TheMinutes mm)         -> enumMinutes mm
    (TheHours hh)           -> enumHours hh
    (TheWeekDaysInWeek ww)  -> enumWeekDaysInWeek ww
    (TheWeekDaysInMonth ww) -> enumWeekDaysInMonth ww
    (TheDays dd)            -> enumDays dd
    (TheMonths mm)          -> enumMonths mm
    (TheYearDays yy)        -> enumYearDays yy
eval' (Filter x) = case x of
    (OnSeconds ss)  -> filterSeconds ss
    (OnMinutes mm)  -> filterMinutes mm
    (OnHours hh)    -> filterHours hh
    (OnWeekDays ww) -> filterWeekDays ww
    (OnDays dd)     -> filterDays dd
    (OnMonths mm)   -> filterMonths mm
    (OnYearDays yy) -> filterYearDays yy
eval' (Select x) = case x of
    (FromSeconds ss)         -> nthSecond ss
    (FromMinutes mm)         -> nthMinute mm
    (FromHours hh)           -> nthHour hh
    (FromWeekDaysInWeek ww)  -> nthWeekDayOfWeek ww
    (FromWeekDaysInMonth ww) -> nthWeekDayOfMonth ww
    (FromDays dd)            -> nthDay dd
    (FromMonths mm)          -> nthDay mm
    (FromYearDays yy)        -> nthYearDay yy
eval' ((:&) x y)   = eval' x >=> eval' y
eval' ((:|) x y)   = eval' x >=> eval' y
eval' ((:!!) x y)  = eval' x >=> eval' y
eval' ((:>) x y)   = eval' x >=> eval' y
eval' ((:>>) x y)  = eval' x >=> eval' y
eval' ((:>>>) x y) = eval' x >=> eval' y

eval :: (CalendarTimeConvertible a, Ord a, Moment a) => Schedule b -> (a -> [a])
eval (Then recur details) = flip (starting $ mkIM recur) $ eval' details
eval recur@(Recur _)      = begin $ mkIM recur

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

-- | 'starting' is an infinite list of 'Moment's, where no 'Moment' 
-- occurrs before the 'InitialMoment'. The list is further refined
-- by the passed in function.
starting :: (Ord a, Moment a) => 
  InitialMoment a 
  -> a 
  -> ([a] -> FutureMoments a)
  -> [a]
starting im m0 = dropWhile (< m0) . O.nub . iterateFutureMoments im{moment=m0}

begin :: (Ord a, Moment a) => InitialMoment a -> a -> [a]
begin im m0 = starting im m0 return
