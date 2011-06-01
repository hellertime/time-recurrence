module Data.Time.Moment.Moment
    (
      -- * Moment
      Moment (..)
    , iterateMoments

      -- * Initial Moment
    , InitialMoment (..)
    , secondly
    , minutely
    , hourly
    , daily
    , weekly
    , monthly
    , yearly

      -- * Frequency
    , Frequency (..)

    , Interval (fromInterval)
    , toInterval

    , StartOfWeek (fromStartOfWeek)
    , toStartOfWeek
    )
  where

import Data.Time.Calendar.WeekDay

oneSecond :: Integer
oneSecond = 1

oneMinute :: Integer
oneMinute = 60 * oneSecond

oneHour :: Integer
oneHour   = 60 * oneMinute

oneDay :: Integer
oneDay    = 24 * oneHour

oneWeek :: Integer
oneWeek   = 7  * oneDay

-- | The @Moment@ class is for representing a instance in time.
--
-- Instances of @Moment@ can be derived for any user-defined
-- datatype for which can satisfy the minimal complete definition.
--
-- Minimal complete definition: 'epoch', 'addSeconds', 'addMonths', 'addYears'

class Moment a where
  -- | Provide a default moment.
  epoch      :: a
  addSeconds :: a -> Integer -> a
  addMonths  :: a -> Integer -> a
  addYears   :: a -> Integer -> a

  -- | Produce a new @Moment@ in the future ocurring at (/interval/ * /freq/)
  next :: Interval -> Frequency -> a -> a
  next (Interval interval) freq =
    case freq of
      Seconds -> add oneSecond
      Minutes -> add oneMinute
      Hours   -> add oneHour
      Days    -> add oneDay
      Weeks   -> add oneWeek
      Months  -> flip addMonths interval
      Years   -> flip addYears interval
    where
      add x = flip addSeconds (interval * x)

  -- | Produce a new @Moment@ in the past ocurring at (-/interval/ * /freq/)
  prev :: Interval -> Frequency -> a -> a
  prev (Interval interval) = next $ Interval (-interval)

-- | Produce an infinite list from an initial @Moment@ and a step function.
iterateMoments :: Moment a => (a -> a) -> a -> [a]
iterateMoments = iterate

-- | The @InitialMoment@ datatype

data InitialMoment a = InitialMoment
    { frequency   :: Frequency
    , interval    :: Interval
    , startOfWeek :: StartOfWeek
    , moment      :: a
    }
  deriving (Show)

mkIM :: Moment a => Frequency -> InitialMoment a
mkIM f = InitialMoment f (toInterval 1) (StartOfWeek Monday) epoch

-- | Default initial moments

secondly :: Moment a => InitialMoment a
secondly = mkIM Seconds

minutely :: Moment a => InitialMoment a
minutely = mkIM Minutes

hourly :: Moment a => InitialMoment a
hourly = mkIM Hours

daily :: Moment a => InitialMoment a
daily = mkIM Days

weekly :: Moment a => InitialMoment a
weekly = mkIM Weeks

monthly :: Moment a => InitialMoment a
monthly = mkIM Months

yearly :: Moment a => InitialMoment a
yearly = mkIM Years

-- | @Frequency@ data type
data Frequency
    = Seconds
    | Minutes
    | Hours
    | Days
    | Weeks
    | Months
    | Years
  deriving (Show)

newtype Interval = Interval { fromInterval :: Integer } deriving (Show)

toInterval :: Integer -> Interval
toInterval = Interval

newtype StartOfWeek = StartOfWeek { fromStartOfWeek :: WeekDay } deriving (Show)

toStartOfWeek :: WeekDay -> StartOfWeek
toStartOfWeek = StartOfWeek
