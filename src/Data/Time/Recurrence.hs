module Data.Time.Recurrence
    (
      -- ^ Data Types
      WeekDay (..)
    , Month (..)

      -- ^ Default Constructors
    , secondly
    , minutely
    , hourly
    , daily
    , weekly
    , monthly
    , yearly
    , secondlyWith
    , minutelyWith
    , hourlyWith
    , dailyWith
    , weeklyWith
    , monthlyWith
    , yearlyWith

      -- ^ API
    , next
    )
  where


import Data.Time
import Data.Time.Calendar.MonthDay (monthLength)
import Data.Time.Calendar.OrdinalDate (mondayStartWeek, toOrdinalDate)


-- | The base frequency of a Recurrence.
--
-- The frequencies are chosen fromRFC 5545.
data Frequencey =
      Secondly    -- ^ every second
    | Minutely    -- ^ every minute
    | Hourly      -- ^ every hour
    | Daily       -- ^ every day
    | Weekly      -- ^ every week
    | Monthly     -- ^ every month
    | Yearly      -- ^ every year
  deriving (Show, Eq)


-- | Symbolic week days.
--
-- Note: The first Day of the Week is Monday 
-- TODO: Move this to a more general library
data WeekDay =
      Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
  deriving (Show, Eq, Ord, Enum, Bounded)


-- | Symbolic months.
--
-- TODO: Move this to a more general library
data Month =
      January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December
  deriving (Show, Eq, Ord, Bounded)

instance Enum Month where
  fromEnum January   = 1
  fromEnum February  = 2
  fromEnum March     = 3
  fromEnum April     = 4
  fromEnum May       = 5
  fromEnum June      = 6
  fromEnum July      = 7
  fromEnum August    = 8
  fromEnum September = 9
  fromEnum October   = 10
  fromEnum November  = 11
  fromEnum December  = 12

  toEnum 1  = January
  toEnum 2  = February
  toEnum 3  = March
  toEnum 4  = April
  toEnum 5  = May
  toEnum 6  = June
  toEnum 7  = July
  toEnum 8  = August
  toEnum 9  = September
  toEnum 10 = October
  toEnum 11 = November
  toEnum 12 = December

  toEnum unmatched = error ("Month.toEnum: Cannot match " ++ show unmatched)


type Refinement = (Eq a) => Maybe ((Moment -> a) -> [a] -> Recurrence -> Maybe Recurrence)

-- | Recurrence data type
--
-- This encapulates a point-in-time of a recurring series
-- from here we can compute the 'next' and 'prev' Recurrence
data Recurrence = R
    { pointInTime  :: UTCTime    -- ^ the current moment
    , startOfWeek  :: WeekDay    -- ^ used in calculations on a week
                                 --   TODO: compute accurately when this
                                 --         is another value than Monday
                                 -- ^ (RFC 5545 defaults to Monday)
    , frequency    :: Frequency  -- ^ defines the base frequency
    , interval     :: Integer    -- ^ defines the step size 
    , rollOver     :: Bool       -- ^ for values that would exceed the base
                                 --   frequency, we clip them to the freq
                                 --   unless this is True
    , refinement   :: Refinement -- ^ A possible function which will modify
                                 --   the recurrence pattern from the default
                                 --   frequency
    }
  deriving (Show)

instance Eq Recurrence where
  x == y = (pointInTime x) == (pointInTime y)


-- | Moment data type
--
-- Refinements work on a Moment, which is a UTCTime broken out into fields
data Moment =
    { year        :: Integer     -- ^ YYYY-January-dd
    , month       :: Month       -- 
    , day         :: Int         -- 
    , hour        :: Int         -- ^ HH:MM:SS
    , minute      :: Int         -- 
    , second      :: Int         -- 
    , week        :: Int         --
    , weekDay     :: WeekDay     --
    , yearDay     :: Int         --
    , daysInMonth :: Int         --
    , leapYear    :: Bool        --
    , timeZone    :: TimeZone    --
    }
  deriving (Show)


-- | moment conversions
moment :: a -> Moment
moment (R time _ _ _ _ _) = moment time
moment (UTCTime d t) = Moment year (toEnum month) day hour minute (fromEnum second) week (toEnum weekDay) yearDay daysInMonth leapYear utc
  where
    (year, month, day) = toGregorian d
    (TimeOfDay hour minute second) = timeToTimeOfDay t
    (week, weekDay) = mondayStartWeek d
    yearDay = snd $ toOrdinalDate d
    leapYear = isLeapYear year
    daysInMonth = monthLength leapYear month


-- | UTCTime zero
utcEpoch = UTCTime (toEnum 0) 0


-- Default constructor. Use a more refined constructor instead
mkR frequency refinement = R utcEpoch Monday frequency 1 False refinement


-- | Base 'Recurrence' constructors. One for each of the Frequency types.
--   Two versions are provided, one that defaults to no refinement and one
--   that requires a refinement be specified.
secondlyWith = R Secondly
secondly = secondlyWith Nothing

minutelyWith = R Minutely
minutely = minutelyWith Nothing

hourlyWith = R Hourly
hourly = hourlyWith Nothing

dailyWith = R Daily
daily = dailyWith Nothing

weeklyWith = R Weekly
weekly = weeklyWith Nothing

monthlyWith = R Monthly
monthly = monthlyWith Nothing

yearlyWith = R Yearly
yearly = yearlyWith Nothing


-- useful time constants
oneSecond = 1
oneMinute = 60 * oneSecond
oneHour   = 60 * oneMinute
oneDay    = 24 * oneHour
oneWeek   = 7  * oneDay


-- UTCTime addition
addTime :: Integer -> UTCTime -> UTCTime
addTime i = addUTCTime (fromIntegral i)

addDays :: (Integer -> Day -> Day) -> Integer -> UTCTime -> UTCTime
addDays f i (UTCTime d t) = UTCTime (f i d) t

addMonthsClip = addDays addGregorianMonthsClip
addMonthsRollOver = addDays addGregorianMonthsRollOver

addYearsClip = addDays addGregorianYearsClip
addYearsRollOver = addDays addGregorianYearsRollOver


-- | Recurrence addition
scalePointInTime :: (Integer -> UTCTime -> UTCTime) -> Integer -> Recurrence -> Recurrence
scalePointInTime f m r@(R t _ _ i _ _) = r { pointInTime = (f (i * m) t) }


-- | Forward and backward drivers.
next :: Recurrence -> Recurrence
next r@(R _ Secondly _ _ _ Nothing) = scalePointInTime addTime oneSecond r
next r@(R _ Minutely _ _ _ Nothing) = scalePointInTime addTime oneMinute r
next r@(R _ Hourly   _ _ _ Nothing) = scalePointInTime addTime oneHour   r
next r@(R _ Daily    _ _ _ Nothing) = scalePointInTime addTime oneDay    r
next r@(R _ Weekly   _ _ _ Nothing) = scalePointInTime addTime oneWeek   r
next r@(R _ Monthly  _ _ r Nothing) =
  case r of
    True  -> scalePointInTime addMonthsRollOver 1 r
    False -> scalePointInTime addMonthsClip     1 r
next r@(R _ Yearly   _ _ r Nothing) =
  case r of
    True  -> scalePointInTime addYearsRollOver 1 r
    False -> scalePointInTime addYearsClip     1 r
