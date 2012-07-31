{-# LANGUAGE GADTs, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, StandaloneDeriving #-}
module Data.Time.Recurrence.ScheduleDetails
    (
      -- * ScheduleDetails
      ScheduleDetails (..)

      -- * NominalSet typeclass
    , NominalSet

      -- * newtype wrappers from non-distinct period types
    , Second
    , Minute
    , Hour
    , Day
    , YearDay
    , WeekDayInWeek
    , WeekDayInMonth

      -- * EnumerablePeriod
    , EnumerablePeriod

      -- * EPeriod
    , EPeriod (..)

      -- * FilterablePeriod
    , FilterablePeriod

      -- * FPeriod
    , FPeriod (..)

      -- * SelectablePeriod
    , SelectablePeriod

      -- * SPeriod
    , SPeriod (..)
    )
  where

import Data.Time.Calendar.Month
import Data.Time.Calendar.WeekDay

infixr 5 :&, :| --, :!!
infixr 0 :> --, :>>, :>>>

data ScheduleDetails a where
    Enumerate :: (EnumerablePeriod a b) => a -> ScheduleDetails (EPeriod b)
    Filter    :: (FilterablePeriod a b) => a -> ScheduleDetails (FPeriod b)
--    Select    :: (SelectablePeriod a)   => a -> ScheduleDetails SPeriod
    (:&)      :: ScheduleDetails (EPeriod a) -> ScheduleDetails (EPeriod a) -> ScheduleDetails (EPeriod a)
    (:|)      :: ScheduleDetails (FPeriod a) -> ScheduleDetails (FPeriod a) -> ScheduleDetails (FPeriod a)
--    (:!!)     :: ScheduleDetails SPeriod     -> ScheduleDetails SPeriod     -> ScheduleDetails SPeriod
    (:>)      :: ScheduleDetails (EPeriod a) -> ScheduleDetails (FPeriod b) -> ScheduleDetails (FPeriod b)
--    (:>>)     :: ScheduleDetails (FPeriod a) -> ScheduleDetails SPeriod     -> ScheduleDetails SPeriod
--    (:>>>)    :: ScheduleDetails (EPeriod a) -> ScheduleDetails SPeriod     -> ScheduleDetails SPeriod

deriving instance Show (ScheduleDetails a)

newtype Second         = Second         { fromSecond  :: Int }            deriving (Eq, Ord, Read, Show)
newtype Minute         = Minute         { fromMinute  :: Int }            deriving (Eq, Ord, Read, Show)
newtype Hour           = Hour           { fromHour    :: Int }            deriving (Eq, Ord, Read, Show)
newtype Day            = Day            { fromDay     :: Int }            deriving (Eq, Ord, Read, Show)
newtype YearDay        = YearDay        { fromYearDay :: Int }            deriving (Eq, Ord, Read, Show)
newtype WeekDayInWeek  = WeekDayInWeek  { fromWeekDayInWeek :: WeekDay }  deriving (Eq, Ord, Read, Show)
newtype WeekDayInMonth = WeekDayInMonth { fromWeekDayInMonth :: WeekDay } deriving (Eq, Ord, Read, Show)

class NominalSet a b | b -> a where
  toNominalSet :: [a] -> [b]
  fromNominalSet :: [b] -> [a]

instance NominalSet Int Second where
  toNominalSet = map (Second)
  fromNominalSet = map (fromSecond)

instance NominalSet Int Minute where
  toNominalSet = map (Minute)
  fromNominalSet = map (fromMinute)

instance NominalSet Int Hour where
  toNominalSet = map (Hour)
  fromNominalSet = map (fromHour)

instance NominalSet Int Day where
  toNominalSet = map (Day)
  fromNominalSet = map (fromDay)

instance NominalSet Int YearDay where
  toNominalSet = map (YearDay)
  fromNominalSet = map (fromYearDay)

instance NominalSet WeekDay WeekDayInWeek where
  toNominalSet = map (WeekDayInWeek)
  fromNominalSet = map (fromWeekDayInWeek)

instance NominalSet WeekDay WeekDayInMonth where
  toNominalSet = map (WeekDayInMonth)
  fromNominalSet = map (fromWeekDayInMonth)

instance NominalSet WeekDay WeekDay where
  toNominalSet = id
  fromNominalSet = id

instance NominalSet Month Month where
  toNominalSet = id
  fromNominalSet = id

data EPeriod a
    = EPSeconds [a]
    | EPMinutes [a]
    | EPHours [a]
    | EPWeekDaysInWeek [a]
    | EPWeekDaysInMonth [a]
    | EPDays [a]
    | EPMonths [a]
    | EPYearDays [a]
  deriving (Read, Show)

class (NominalSet a b) => EnumerablePeriod a b where
  on :: [a] -> ScheduleDetails (EPeriod b)

instance EnumerablePeriod Int Second where
  on = Enumerate . EPSeconds . toNominalSet

instance EnumerablePeriod Int Minute where
  on = Enumerate . EPMinutes . toNominalSet

instance EnumerablePeriod Int Hour where
  on = Enumerate . EPHours . toNominalSet

instance EnumerablePeriod WeekDay WeekDayInWeek where
  on = Enumerate . EPWeekDaysInWeek . toNominalSet

instance EnumerablePeriod WeekDay WeekDayInMonth where
  on = Enumerate . EPWeekDaysInMonth . toNominalSet

instance EnumerablePeriod Int Day where
  on = Enumerate . EPDays . toNominalSet

instance EnumerablePeriod Month Month where
  on = Enumerate . EPMonths . toNominalSet

instance EnumerablePeriod Int YearDay where
  on = Enumerate . EPYearDays . toNominalSet

data FPeriod a
    = FPSeconds [a]
    | FPMinutes [a]
    | FPHours [a]
    | FPWeekDays [a]
    | FPDays [a]
    | FPMonths [a]
    | FPYearDays [a]
  deriving (Read, Show)

class (NominalSet a b) => FilterablePeriod a b where
  filter :: [a] -> ScheduleDetails (FPeriod b)

instance FilterablePeriod Int Second where
  filter = Filter . FPSeconds . toNominalSet

instance FilterablePeriod Int Minute where
  filter = Filter . FPMinutes . toNominalSet

instance FilterablePeriod Int Hour where
  filter = Filter . FPHours . toNominalSet

instance FilterablePeriod WeekDay WeekDay where
  filter = Filter . FPWeekDays . toNominalSet

instance FilterablePeriod Int Day where
  filter = Filter . FPDays . toNominalSet

instance FilterablePeriod Month Month where
  filter = Filter . FPMonths . toNominalSet

instance FilterablePeriod Int YearDay where
  filter = Filter . FPYearDays . toNominalSet

{-
data SPeriod
    = SPSeconds [Int]
    | SPMinutes [Int]
    | SPHours [Int]
    | SPWeekDaysInWeek [Int]
    | SPWeekDaysInMonth [Int]
    | SPDays [Int]
    | SPMonths [Int]
    | SPYearDays [Int]
  deriving (Read, Show)

class SelectablePeriod a where
  select :: [Int] -> ScheduleDetails SPeriod

instance SelectablePeriod Second where
  select = Select . SPSeconds 

instance SelectablePeriod Minute where
  select = Select . SPMinutes

instance SelectablePeriod Hour where
  select = Select . SPHours

instance SelectablePeriod WeekDayInWeek where
  select = Select . SPWeekDaysInWeek

instance SelectablePeriod WeekDayInMonth where
  select = Select . SPWeekDaysInMonth

instance SelectablePeriod Day where
  select = Select . SPDays

instance SelectablePeriod Month where
  select = Select . SPMonths

instance SelectablePeriod YearDay where
  select = Select . SPYearDays
-}
