{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, RankNTypes, StandaloneDeriving, TypeSynonymInstances,UndecidableInstances #-}
module Data.Time.Recurrence.ScheduleDetails
    (
      -- * ScheduleDetails
      ScheduleDetails

    , eval

      -- * Functional interface to constructors
    , enum
    , filter
    , select

      -- * Period Filters
    , PeriodFilter (..)
    , EnumerablePeriodFilter (..)
    , FilterablePeriodFilter (..)
    , SelectablePeriodFilter (..)
    )
  where

import Prelude hiding (filter)
import Control.Monad ((>=>))
import Data.Time.Calendar.Month
import Data.Time.Calendar.WeekDay
import Data.Time.CalendarTime
import Data.Time.Moment hiding (Period(..))
import Data.Time.Recurrence.AndThen

data ScheduleDetails a where
    Enumerate  :: EnumerablePeriodFilter -> ScheduleDetails EnumerablePeriodFilter
    Filter     :: FilterablePeriodFilter -> ScheduleDetails FilterablePeriodFilter
    Select     :: SelectablePeriodFilter -> ScheduleDetails SelectablePeriodFilter
    EPFCons    :: ScheduleDetails EnumerablePeriodFilter -> ScheduleDetails EnumerablePeriodFilter -> ScheduleDetails EnumerablePeriodFilter
    FPFCons    :: ScheduleDetails FilterablePeriodFilter -> ScheduleDetails FilterablePeriodFilter -> ScheduleDetails FilterablePeriodFilter
    SPFCons    :: ScheduleDetails SelectablePeriodFilter -> ScheduleDetails SelectablePeriodFilter -> ScheduleDetails SelectablePeriodFilter
    EPFConsFPF :: ScheduleDetails EnumerablePeriodFilter -> ScheduleDetails FilterablePeriodFilter -> ScheduleDetails FilterablePeriodFilter
    FPFConsSPF :: ScheduleDetails FilterablePeriodFilter -> ScheduleDetails SelectablePeriodFilter -> ScheduleDetails SelectablePeriodFilter
    EPFConsSPF :: ScheduleDetails EnumerablePeriodFilter -> ScheduleDetails SelectablePeriodFilter -> ScheduleDetails SelectablePeriodFilter

deriving instance Show (ScheduleDetails a)

enum :: PeriodFilter Month WeekDay NotEnumerable -> ScheduleDetails EnumerablePeriodFilter
enum = Enumerate . EPF

filter :: PeriodFilter Month NotFilterable WeekDay -> ScheduleDetails FilterablePeriodFilter
filter = Filter . FPF

select :: PeriodFilter Int Int Int -> ScheduleDetails SelectablePeriodFilter
select = Select . SPF

type BareEPF = EnumerablePeriodFilter
type WrapEPF = ScheduleDetails EnumerablePeriodFilter

instance AndThen BareEPF BareEPF WrapEPF where
  (>==>) x y = (Enumerate x) `EPFCons` (Enumerate y)

instance AndThen BareEPF WrapEPF WrapEPF where
  (>==>) x y = (Enumerate x) `EPFCons` y

instance AndThen WrapEPF WrapEPF WrapEPF where
  (>==>) x y = x `EPFCons` y

type BareFPF = FilterablePeriodFilter
type WrapFPF = ScheduleDetails FilterablePeriodFilter

instance AndThen BareFPF BareFPF WrapFPF where
  (>==>) x y = (Filter x) `FPFCons` (Filter y)

instance AndThen BareFPF WrapFPF WrapFPF where
  (>==>) x y = (Filter x) `FPFCons` y

instance AndThen WrapFPF WrapFPF WrapFPF where
  (>==>) x y = x `FPFCons` y

type BareSPF = SelectablePeriodFilter
type WrapSPF = ScheduleDetails SelectablePeriodFilter

instance AndThen BareSPF BareSPF WrapSPF where
  (>==>) x y = (Select x) `SPFCons` (Select y)

instance AndThen BareSPF WrapSPF WrapSPF where
  (>==>) x y = (Select x) `SPFCons` y

instance AndThen WrapSPF WrapSPF WrapSPF where
  (>==>) x y = x `SPFCons` y

instance AndThen WrapEPF WrapFPF WrapFPF where
  (>==>) x y = x `EPFConsFPF` y

instance AndThen WrapFPF WrapSPF WrapSPF where
  (>==>) x y = x `FPFConsSPF` y

instance AndThen WrapEPF WrapSPF WrapSPF where
  (>==>) x y = x `EPFConsSPF` y

data PeriodFilter m e f
    = Seconds [Int]
    | Minutes [Int]
    | Hours [Int]
    | Days [Int]
    | Weeks [Int]
    | WeekDays [f]
    | WeekDaysInWeek [e]
    | WeekDaysInMonth [e]
    | Months [m]
    | YearDays [Int]
  deriving (Read, Show)

data NotEnumerable
data NotFilterable

instance Show NotEnumerable where
  show _ = undefined

instance Read NotEnumerable where
  readsPrec _ _ = undefined

instance Show NotFilterable where
  show _ = undefined

instance Read NotFilterable where
  readsPrec _ _ = undefined

newtype EnumerablePeriodFilter = EPF { fromEPF :: PeriodFilter Month WeekDay NotEnumerable } deriving (Read, Show)
newtype FilterablePeriodFilter = FPF { fromFPF :: PeriodFilter Month NotFilterable WeekDay } deriving (Read, Show)
newtype SelectablePeriodFilter = SPF { fromSPF :: PeriodFilter Int Int Int } deriving (Read, Show)

eval :: (CalendarTimeConvertible a, Ord a, Moment a) => ScheduleDetails b -> ([a] -> FutureMoments a)
eval (Enumerate x) = case (fromEPF x) of
    (Seconds ss)         -> enumSeconds ss
    (Minutes mm)         -> enumMinutes mm
    (Hours hh)           -> enumHours hh
    (WeekDaysInWeek ww)  -> enumWeekDaysInWeek ww
    (WeekDaysInMonth ww) -> enumWeekDaysInMonth ww
    (Days dd)            -> enumDays dd
    (Months mm)          -> enumMonths mm
    (YearDays yy)        -> enumYearDays yy
eval (Filter x) = case (fromFPF x) of
    (Seconds ss)  -> filterSeconds ss
    (Minutes mm)  -> filterMinutes mm
    (Hours hh)    -> filterHours hh
    (WeekDays ww) -> filterWeekDays ww
    (Days dd)     -> filterDays dd
    (Months mm)   -> filterMonths mm
    (YearDays yy) -> filterYearDays yy
eval (Select x) = case (fromSPF x) of
    (Seconds ss)         -> nthSecond ss
    (Minutes mm)         -> nthMinute mm
    (Hours hh)           -> nthHour hh
    (WeekDaysInWeek ww)  -> nthWeekDayOfWeek ww
    (WeekDaysInMonth ww) -> nthWeekDayOfMonth ww
    (Days dd)            -> nthDay dd
    (Months mm)          -> nthDay mm
    (YearDays yy)        -> nthYearDay yy
eval (EPFCons x y)    = eval x >=> eval y
eval (FPFCons x y)    = eval x >=> eval y
eval (SPFCons x y)    = eval x >=> eval y
eval (EPFConsFPF x y) = eval x >=> eval y
eval (FPFConsSPF x y) = eval x >=> eval y
eval (EPFConsSPF x y) = eval x >=> eval y


