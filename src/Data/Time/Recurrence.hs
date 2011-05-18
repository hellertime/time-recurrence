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

      -- ^ API
    , recur
    , withRules
    , byMonth
    , byWeekNumber
    , byYearDay
    )
  where


import Data.List.Ordered (nub, nubSort)
import Data.Ord (comparing)
import Data.Time
import Data.Time.Calendar.MonthDay (monthLength)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate, toOrdinalDate)
import Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)


-- | The base frequency of a Recurrence.
--
-- The frequencies are chosen fromRFC 5545.
data Frequency =
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


-- | Recurrence data type
--
-- This encapulates a point-in-time of a recurring series
-- from here we can compute the 'next' and 'prev' Recurrence
data Recurrence = R
    { frequency    :: Frequency    -- ^ defines the base frequency
    , pointInTime  :: UTCTime      -- ^ the current moment
    , startOfWeek  :: WeekDay      -- ^ used in calculations on a week
                                   --   TODO: compute accurately when this
                                   --         is another value than Monday
                                   -- ^ (RFC 5545 defaults to Monday)
    , interval     :: Integer      -- ^ defines the step size 
    , rollOver     :: Bool         -- ^ for values that would exceed the base
                                   --   frequency, we clip them to the freq
                                   --   unless this is True
    }
  deriving (Show)

instance Eq Recurrence where
  x == y = pointInTime x == pointInTime y

instance Ord Recurrence where
  x `compare` y = comparing pointInTime x y


-- | Moment data type
--
-- Refinements work on a Moment, which is a UTCTime broken out into fields
data Moment = M
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
moment :: UTCTime -> Moment
moment (UTCTime d t) = M year (toEnum month) day hour minute (fromEnum second) week (toEnum weekDay) yearDay daysInMonth leapYear utc
  where
    (year, month, day) = toGregorian d
    (TimeOfDay hour minute second) = timeToTimeOfDay t
    (_, week, weekDay) = toWeekDate d
    yearDay = snd $ toOrdinalDate d
    leapYear = isLeapYear year
    daysInMonth = monthLength leapYear month


momentToUTCTime :: Moment -> UTCTime
momentToUTCTime m = UTCTime d t
  where
    d = fromGregorian (year m) (fromEnum $ month m) (day m)
    t = timeOfDayToTime $ TimeOfDay (hour m) (minute m) (toEnum $ second m)

-- | UTCTime zero
utcEpoch = UTCTime (toEnum 0) 0


-- Default constructor. Use a more refined constructor instead
mkR frequency = R frequency utcEpoch Monday 1 False


-- | Base 'Recurrence' constructors. One for each of the Frequency types.
secondly = mkR Secondly
minutely = mkR Minutely
hourly = mkR Hourly
daily = mkR Daily
weekly = mkR Weekly
monthly = mkR Monthly
yearly = mkR Yearly


-- useful time constants
oneSecond = 1
oneMinute = 60 * oneSecond
oneHour   = 60 * oneMinute
oneDay    = 24 * oneHour
oneWeek   = 7  * oneDay


-- UTCTime addition
addTime :: Integer -> UTCTime -> UTCTime
addTime i = addUTCTime (fromIntegral i)

addUTCDays :: (Integer -> Day -> Day) -> Integer -> UTCTime -> UTCTime
addUTCDays f i (UTCTime d t) = UTCTime (f i d) t

addMonthsClip = addUTCDays addGregorianMonthsClip
addMonthsRollOver = addUTCDays addGregorianMonthsRollOver

addMonths b = if b then addMonthsRollOver else addMonthsClip

addYearsClip = addUTCDays addGregorianYearsClip
addYearsRollOver = addUTCDays addGregorianYearsRollOver

addYears b = if b then addYearsRollOver else addYearsClip


-- | Recurrence addition
scalePointInTime :: (Integer -> UTCTime -> UTCTime) -> Integer -> Recurrence -> Recurrence
scalePointInTime f m r@(R _ t _ i _) = r {pointInTime = f (i * m) t}


-- | Forward and backward drivers.
next :: Recurrence -> Recurrence
next r@(R Secondly _ _ _ _) = scalePointInTime addTime oneSecond r
next r@(R Minutely _ _ _ _) = scalePointInTime addTime oneMinute r
next r@(R Hourly   _ _ _ _) = scalePointInTime addTime oneHour   r
next r@(R Daily    _ _ _ _) = scalePointInTime addTime oneDay    r
next r@(R Weekly   _ _ _ _) = scalePointInTime addTime oneWeek   r
next r@(R Monthly  _ _ _ roll) = scalePointInTime (addMonths roll) 1 r
next r@(R Yearly   _ _ _ roll) = scalePointInTime (addYears roll) 1 r

-- | Generate a infinite list of recurrences
recur :: Recurrence -> [Recurrence]
recur = iterate next


-- | Takes a list of rule parts and apply them over a list of Recurrence
--   to generate a new list of Recurrence
withRules :: [Recurrence -> [Recurrence]] -> [Recurrence] -> [Recurrence]
withRules rs = nub . mapply (map concatMap rs)
  where
    mapply fs xs = foldl (\xs f -> f xs) xs fs


byMonth :: [Month] -> Recurrence -> [Recurrence]
byMonth ms = go
  where
    go :: Recurrence -> [Recurrence]
    go r@(R Yearly t _ _ _) = map (expand r $ moment t) $ nubSort ms
    go r@(R _ t _ _ _) = [r | month (moment t) `elem` ms]
    expand :: Recurrence -> Moment -> Month -> Recurrence
    expand r mt m = r { pointInTime = momentToUTCTime $ mt { month = m } }

byWeekNumber :: [Int] -> Recurrence -> [Recurrence]
byWeekNumber wks = go
  where
    go :: Recurrence -> [Recurrence]
    go r@(R Yearly t _ _ _) = map (expand r $ moment t) $ nubSort wks
    go r = error ("Data.Time.Recurrence.byWeekNumber: Undefined on " ++ show r)
    expand :: Recurrence -> Moment -> Int -> Recurrence
    expand r mt wk = r {pointInTime = momentToUTCTime $ mt {year = y, month = toEnum m, day = d}}
      where
        (y, m, d) = toGregorian $ fromWeekDate (year mt) wk (day mt)

byYearDay :: [Int] -> Recurrence -> [Recurrence]
byYearDay dys = go
  where
    normDay :: Int -> Int
    normDay d = if d < 0 then 367 + d else d
    normDays = nubSort $ map normDay dys
    go :: Recurrence -> [Recurrence]
    go r@(R f t _ _ _) =
      case f of
        Yearly -> map (expand r $ moment t) normDays
        Hourly   -> limit t r
        Minutely -> limit t r
        Secondly -> limit t r
        _ -> error ("Data.Time.Recurrence.byYearDay: Undefined on " ++ show r)
    limit :: UTCTime -> Recurrence -> [Recurrence]
    limit t r = [r | yearDay (moment t) `elem` dys]
    expand :: Recurrence -> Moment -> Int -> Recurrence
    expand r mt dy = r {pointInTime = momentToUTCTime $ mt {year = y, month = toEnum m, day = d}}
      where
        (y, m, d) = toGregorian $ fromOrdinalDate (year mt) dy
