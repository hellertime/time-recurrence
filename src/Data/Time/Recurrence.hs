module Data.Time.Recurrence
    (
      -- * The @WeekDay@ type
      WeekDay (..)

      -- * The @Month@ type
    , Month (..)

      -- * The @Moment@ type class
    , Moment (..)

      -- * create types in @RecurrenceParameters@
    , toInterval
    , toStartOfWeek

    )
  where

import Control.Applicative
import Control.Monad.Reader
import Data.List.Ordered (nubSort)
import Data.Maybe (mapMaybe)
import Data.Time
import Data.Time.Calendar.MonthDay (monthLength)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate, fromOrdinalDateValid, fromMondayStartWeekValid, mondayStartWeek)
import Data.Traversable

-- | Symbolic week days.
--
-- Note: The first Day of the Week is Monday 
-- TODO: Move this to a more general library
data WeekDay
    = Monday
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
data Month
    = January
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

-- | @DateTime@ data type
--   This is a componentized version of a time value
--   simmilar to a 'struct tm'
data DateTime = DateTime
    { dtSecond   :: Int
    , dtMinute   :: Int
    , dtHour     :: Int
    , dtDay      :: Int
    , dtMonth    :: Month
    , dtYear     :: Integer
    , dtWeekDay  :: WeekDay
    , dtYearDay  :: Int
    , dtTimeZone :: TimeZone
    }
  deriving (Show)

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

newtype Interval = Interval Integer deriving (Show)
newtype StartOfWeek = StartOfWeek WeekDay deriving (Show)

toInterval :: Integer -> Interval
toInterval = Interval

toStartOfWeek :: WeekDay -> StartOfWeek
toStartOfWeek = StartOfWeek

-- useful time constants
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
-- Minimal complete definition: 'epoch', 'toDateTime', 'fromDateTime',
-- 'scaleTime', 'scaleMonth', 'scaleYear', 'alterWeekNumber',
-- 'alterYearDay'

class Moment a where

  -- | Provide a default moment.
  epoch           :: a

  -- | Convert a @Moment@ into a @DateTime@
  toDateTime      :: a -> DateTime

  -- | Convert a @DateTime@ into a @Moment@
  fromDateTime    :: DateTime -> Maybe a

  -- | Produce a new @Moment@ offset by a given number of seconds.
  scaleTime       :: a -> Integer -> a

  -- | Produce a new @Moment@ offset by a given number of months.
  scaleMonth      :: a -> Integer -> a

  -- | Produce a new @Moment@ offset by a given number of years.
  scaleYear       :: a -> Integer -> a

  -- | Possibly produce a new @Moment@ shifted to a different week of the year.
  alterWeekNumber :: StartOfWeek -> a -> Int -> Maybe a

  -- | Possibly produce a new @Moment@ shifted to a different day of the year.
  alterYearDay    :: a -> Int -> Maybe a

  -- | The 'alter*' methods can potentially produce invalid dates.
  -- 
  -- For each user-defined @Moment@ instance the definitions of
  -- 'toDateTime', 'fromDateTime', 'alterWeekNumber' and 'alterYearDay' 
  -- will determine if an altered @Moment@ that lands on an invalid date
  -- in the given calendar will be reduced to @Nothing@

  -- | Possibly produce a new @Moment@ shifted to a different second of the day.
  alterSecond     :: a -> Int -> Maybe a
  alterSecond x s = fromDateTime (toDateTime x){dtSecond = s}

  -- | Possibly produce a new @Moment@ shifted to a different minute of the day.
  alterMinute     :: a -> Int -> Maybe a
  alterMinute x m = fromDateTime (toDateTime x){dtMinute = m}

  -- | Possibly produce a new @Moment@ shifted to a different hour of the day.
  alterHour       :: a -> Int -> Maybe a
  alterHour x h = fromDateTime (toDateTime x){dtHour = h}

  -- | Possibly produce a new @Moment@ shifted to a different day of the month.
  alterDay        :: a -> Int -> Maybe a
  alterDay x d = fromDateTime (toDateTime x){dtDay = d}

  -- | Possibly produce a new @Moment@ shifted to a different month of the year.
  alterMonth      :: a -> Month -> Maybe a
  alterMonth x m = fromDateTime (toDateTime x){dtMonth = m}

  -- | Possibly produce a new @Moment@ shifted to a different year.
  alterYear       :: a -> Integer -> Maybe a
  alterYear x y = fromDateTime (toDateTime x){dtYear = y}

  -- | Produce a new @Moment@ in the future ocurring at (/interval/ * /freq/)
  next :: Interval -> Frequency -> a -> a
  next (Interval interval) freq =
    case freq of
      Seconds -> scale oneSecond
      Minutes -> scale oneMinute
      Hours   -> scale oneHour
      Days    -> scale oneDay
      Weeks   -> scale oneWeek
      Months  -> flip scaleMonth interval
      Years   -> flip scaleYear interval
    where
      scale x = flip scaleTime (interval * x)

  -- | Produce a new @Moment@ in the past ocurring at (-/interval/ * /freq/)
  prev :: Interval -> Frequency -> a -> a
  prev (Interval interval) = next $ Interval (-interval)

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

-- | The @Schedule@ datatype

newtype Schedule a = Schedule {fromSchedule :: [a]} deriving (Show, Eq, Ord)

-- | Produce an infinite list from an initial @Moment@ and a step function.
iterateMoments :: Moment a => (a -> a) -> a -> [a]
iterateMoments = iterate

type RecurringSchedule a = Reader (InitialMoment a) (Schedule a)

enumMoments :: Moment a => 
               (Interval -> Frequency -> a -> a) 
            -> RecurringSchedule a
enumMoments step = do
  i <- ask
  return $ Schedule $ iterateMoments (step' i) (moment i)
  where
    step' i = step (interval i) (frequency i)

-- | 'enumFutureMoments' is a @Schedule@ of all future moments derived
-- from the @InitialMoment@
enumFutureMoments :: Moment a => RecurringSchedule a
enumFutureMoments = enumMoments next

-- | 'enumPastMoments' goes in the opposite direction of 'enumFutureMoments'
enumPastMoments :: Moment a => RecurringSchedule a
enumPastMoments = enumMoments prev

-- | 'enumPeriod' produces a period of /n/ moments from the 'startDate'.
enumPeriod :: Moment a => Int -> RecurringSchedule a
enumPeriod n = do
  i <- ask
  return $ Schedule $ take n $ iterateMoments step (moment i)
  where
    step = next (interval i) (frequency i)

-- | 'enumYear' produces a period of /n/ days within the current year
enumYear :: Moment a => RecurringSchedule a
enumYear = do
  i <- ask
  if isLeapYear $ dtYear $ toDateTime (moment i)
    then local setPeriod (enumPeriod 365)
    else local setPeriod (enumPeriod 366)
  where
    setPeriod i = let
      m = moment i
      startDate' = max (fromJust $ alterYearDay m 1) (startDate i)
      in i{startDate = startDate', frequency = Days}

-- | 'enumMonths' produces a period of /n/ days withing the current month
enumMonths :: Moment a => RecurringSchedule a
enumMonths = do
  i <- ask
  let dt = toDateTime (moment i)
  let days = monthLength (isLeapYear $ dtYear dt) (fromEnum $ dtMonth dt)
  local setPeriod (enumPeriod days)
  where
    setPeriod i = let
      m = moment i
      startDate' = max (fromJust $ alterDay m 1) (startDate i)
      in i{startDate = startDate', frequency = Days}

-- | Normalize an bounded index
--   Pass an upper-bound 'ub' and an index 'idx'
--   Converts 'idx' < 0 into valid 'idx' > 0 or
--   Nothing
normIndex :: Int -> Int -> Maybe Int
normIndex _ 0 = Nothing
normIndex ub idx =
  if abs idx > ub
    then Nothing
    else Just $ (idx + ub') `mod` ub'
  where
    ub' = ub + 1

mapNormIndex :: Int -> [Int] -> [Int]
mapNormIndex n = mapMaybe (normIndex n)

-- | 'restrict', applied to a predicate and a @Schedule@, returns a @Schedule@
-- of those moments that statisfy the predicate.
restrict :: Moment a => (a -> Bool) -> Schedule a -> RecurringSchedule a
restrict f s = return $ Schedule $ filter f $ fromSchedule s

by :: (Moment a, Ord b) => (DateTime -> b) -> [b] -> a -> Bool
by f bs a = f (toDateTime a) `elem` nubSort bs

by' :: Moment a => (DateTime -> Int) -> Int -> [Int] -> a -> Bool
by' f n bs = by f $ mapNormIndex n bs

bySeconds :: Moment a => [Int] -> a -> Bool
bySeconds = by dtSecond

byMinutes :: Moment a => [Int] -> a -> Bool
byMinutes = by dtMinute

byHours :: Moment a => [Int] -> a -> Bool
byHours = by dtHour

byWeekDays :: Moment a => [WeekDay] -> a -> Bool
byWeekDays = by dtWeekDay

byMonthDays :: Moment a => [Int] -> a -> Bool
byMonthDays = by' dtDay 31

byMonths :: Moment a => [Month] -> a -> Bool
byMonths = by dtMonth

byYearDays :: Moment a => [Int] -> a -> Bool
byYearDays = by' dtYearDay 366

-- monadic concatMap
concatMapM :: Applicative m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> traverse f xs

-- | 'expand', takes an expansion function and a @Schedule@, and maps the
-- expansion function over the moments.
-- Each moment is then replaced with its expansions.
expand :: Moment a => (a -> Reader (InitialMoment a) [a]) -> Schedule a -> RecurringSchedule a
expand f s = do
  xs <- concatMapM f (fromSchedule s)
  return $ Schedule xs

on :: Moment a => 
      (a -> b -> Maybe a) 
   -> [b] 
   -> a 
   -> Reader (InitialMoment a) [a]
on f bs a = return $ mapMaybe (f a) bs

on' :: Moment a =>
       (InitialMoment a -> a -> b -> Maybe a)
     -> [b]
     -> a
     -> Reader (InitialMoment a) [a]
on' f bs a = ask >>= \i -> on (f i) bs a

onMonths :: Moment a => [Month] -> a -> Reader (InitialMoment a) [a]
onMonths = on alterMonth

onYearDays :: Moment a => [Int] -> a -> Reader (InitialMoment a) [a]
onYearDays ds = on alterYearDay (mapNormIndex 366 ds)

onWeekNumbers :: Moment a => [Int] -> a -> Reader (InitialMoment a) [a]
onWeekNumbers ds = on' (alterWeekNumber . startOfWeek) (mapNormIndex 53 ds)

onWeekDays :: Moment a -> [WeekDay] -> a -> Reader (InitialMoment a) [a]
onWeekDays

-- | Instance of the @Moment@ class defined for the @UTCTime@ datatype.

instance Moment UTCTime where
  epoch = UTCTime (toEnum 0) 0
  toDateTime (UTCTime utcDay utcTime) =
    DateTime (fromEnum seconds) minutes hours
             day (toEnum month) year
             weekDay yearDay utc
    where
     (TimeOfDay hours minutes seconds) = timeToTimeOfDay utcTime
     (year, month, day) = toGregorian utcDay
     yearDay = snd $ toOrdinalDate utcDay
     weekDay = toEnum $ snd (mondayStartWeek utcDay) - 1

  fromDateTime dt = do
      let _ = dtTimeZone dt -- just called here to shut GHC up for now
      day <- fromGregorianValid (dtYear dt) (fromEnum $ dtMonth dt) (dtDay dt)
      time <- makeTimeOfDayValid (dtHour dt) (dtMinute dt) (toEnum $ dtSecond dt)
      return $ UTCTime day (timeOfDayToTime time)

  scaleTime utc i = addUTCTime (fromIntegral i) utc
  scaleMonth (UTCTime d t) i = UTCTime (addGregorianMonthsRollOver i d) t
  scaleYear (UTCTime d t) i = UTCTime (addGregorianYearsRollOver i d) t

  -- TODO: First argument is StartOfWeek and is ignored right now. fix.
  alterWeekNumber _ utc@(UTCTime _ time) week = do
    let dt = toDateTime utc
    day <- fromMondayStartWeekValid (dtYear dt) week (fromEnum $ dtWeekDay dt)
    return $ UTCTime day time

  alterYearDay utc@(UTCTime _ time) yearDay = do
    let dt = toDateTime utc
    day <- fromOrdinalDateValid (dtYear dt) yearDay
    return $ UTCTime day time

-- | @InitialMoment@ defaults for @UTCTime@

secondlyUTC :: InitialMoment UTCTime
secondlyUTC = secondly

minutelyUTC :: InitialMoment UTCTime
minutelyUTC = minutely

hourlyUTC :: InitialMoment UTCTime
hourlyUTC = hourly

dailyUTC :: InitialMoment UTCTime
dailyUTC = daily

weeklyUTC :: InitialMoment UTCTime
weeklyUTC = weekly

monthlyUTC :: InitialMoment UTCTime
monthlyUTC = monthly

yearlyUTC :: InitialMoment UTCTime
yearlyUTC = yearly
