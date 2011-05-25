module Data.Time.Recurrence
    (
      -- * The @WeekDay@ type
      WeekDay (..)

      -- * The @Month@ type
    , Month (..)

      -- * The @Moment@ type
    , Moment (..)

      -- * Generate list of recurring @Moment@
    , recurBy
    , recur

      -- * Create @UTCTime@
    , utcGregorian
    , utcGregorianWithTime

      -- * @Recurrence@ type rule effects
    , byMonth
    , byWeekNumber
    , byYearDay
    , byMonthDay
    , byDay 
    )
  where

import Data.List.Ordered (nub, nubSort)
import Data.Maybe (mapMaybe, fromJust)
import Data.Time
import Data.Time.Calendar.MonthDay (monthLength)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate, fromOrdinalDateValid, fromMondayStartWeekValid, mondayStartWeek)

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

newtype Interval = Interval Integer
newtype StartOfWeek = StartOfWeek WeekDay

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

-- | @Moment@ type class
class Moment a where
  -- ^ Minimum Definition
  toDateTime      :: a -> DateTime
  fromDateTime    :: DateTime -> Maybe a
  scaleTime       :: a -> Integer -> a
  scaleMonth      :: a -> Integer -> a
  scaleYear       :: a -> Integer -> a

  alterWeekNumber :: StartOfWeek -> a -> Int -> Maybe a
  alterYearDay    :: a -> Int -> Maybe a

  -- ^ Other Methods

  alterSecond     :: a -> Int -> Maybe a
  alterSecond x s = fromDateTime (toDateTime x){dtSecond = s}

  alterMinute     :: a -> Int -> Maybe a
  alterMinute x m = fromDateTime (toDateTime x){dtMinute = m}

  alterHour       :: a -> Int -> Maybe a
  alterHour x h = fromDateTime (toDateTime x){dtHour = h}

  alterDay        :: a -> Int -> Maybe a
  alterDay x d = fromDateTime (toDateTime x){dtDay = d}

  alterMonth      :: a -> Month -> Maybe a
  alterMonth x m = fromDateTime (toDateTime x){dtMonth = m}

  alterYear       :: a -> Integer -> Maybe a
  alterYear x y = fromDateTime (toDateTime x){dtYear = y}

  next :: Interval -> Frequency -> a -> a
  next interval freq =
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

  prev :: Interval -> Frequency -> a -> a
  prev interval = next (-interval)

instance Moment UTCTime where
  toDateTime (UTCTime utcDay utcTime) =
    DateTime (fromEnum seconds) minutes hours
             year (toEnum month) day
             weekDay yearDay utc
    where
     (TimeOfDay hours minutes seconds) = timeToTimeOfDay utcTime
     (year, month, day) = toGregorian utcDay
     yearDay = snd $ toOrdinalDate utcDay
     weekDay = toEnum $ snd (mondayStarWeek utcDay) - 1

  fromDateTime dt = do
      day <- fromGregorianValid (dtYear dt) (fromEnum $ dtMonth dt) (dtDay dt)
      time <- makeTimeOfDayValid (dtHour dt) (dtMinute dt) (toEnum $ dtSecond dt)
      return $ UTCTime day time

  scaleTime = flip addUTCTime . fromIntegral
  scaleMonth (UTCTime d t) i = UTCTime (addGregorianMonthsRollOver i d) t
  scaleYear (UTCTime d t) i = UTCTime (addGregorianYearsRollOver i d) t

  -- ^ TODO: Stop ignoring 'sow'
  alterWeekNumber sow utc@(UTCTime _ time) week = do
    let dt = toDateTime utc
    day <- fromMondayStartWeek (dtYear dt) week (fromEnum $ dtWeekDay dt)
    return $ UTCTime day time

  alterYearDay utc@(UTCTime _ time) yearDay = do
    let dt = toDateTime utc
    day <- fromOrdinalDateValid (dtYear dt) yearDay
    return $ UTCTime day time

-- | The @RecurrenceParameter@ type
data RecurrenceParameter = RecurrenceParameter
    { interval    :: Interval
    , frequency   :: Frequency
    , startOfWeek :: StartOfWeek
    }
  deriving (Show)

mkRP :: Frequency -> RecurrenceParameter
mkRP f = RecurrenceParameter (Interval 1) f (StartOfWeek Monday)

-- | Some common recurrence parameters
secondly :: RecurrenceParameter -- ^ Recur every second
secondly = mkRP Seconds

minutely :: RecurrenceParameter -- ^ Recur every minute
minutely = mkRP Minutes

hourly :: RecurrenceParameter   -- ^ Recur every hour
hourly = mkRP Hours

daily :: RecurrenceParameter    -- ^ Recur every day
daily = mkRP Days

weekly :: RecurrenceParameter   -- ^ Recur every week
weekly = mkRP Weeks

monthly :: RecurrenceParameter  -- ^ Recur every month
monthly = mkRP Months

yearly :: RecurrenceParameter   -- ^ Recur every year
yearly = mkRP Years

-- | The @Recurrence@ type
--   a @Recurrence@ is on or more @Moment@s
newtype Moment a => Recurrence a = Recurrence [a]

mkR :: Moment a => a -> Recurrence [a]
mkR x = Recurrence [x]

-- | Test if (field t) is elem in xs
momentElem :: Eq a => Moment -> (Time -> a) -> [a] -> Bool
momentElem m field xs = field (utcToTime $ moment m) `elem` xs

-- | Construct a @UTCTime@ at midnight
utcGregorian :: Integer -> Int -> Int -> UTCTime
utcGregorian y m d = UTCTime (fromGregorian y m d) (timeOfDayToTime midnight)

-- | Construct a @UTCTime@ at a time
utcGregorianWithTime :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
utcGregorianWithTime y m d hh mm ss = UTCTime d' t'
  where
    d' = fromGregorian y m d
    t' = timeOfDayToTime (TimeOfDay hh mm (toEnum ss))

-- | Generate recurrences from the startDate, filtered by optional rules
recurBy :: Integer -> [Moment -> [Moment]] -> Moment -> [Moment]
recurBy interval subRules startDate = nub $ applySubRules $ recurFrom startDate
  where
    recurFrom = iterate $ next interval
    fapply fs xs = foldl (\xs' f -> f xs') xs fs
    applySubRules = fapply (map concatMap subRules)

-- | Default interval of 1
recur :: [Moment -> [Moment]] -> Moment -> [Moment]
recur = recurBy 1

-- | Generate all days within the frequency
--   Yearly generates all days in the year
--   Monthly all days in the month of that year
--   Weekly all days in the week of that month of that year,
--   starting on the first day of the week
moments :: Moment a => Frequency -> a -> [a]
moments Years x =
  if isLeapYear $ dtYear $ toDateTime x
    then take 366 $ recur [] $ daily startDate
    else take 365 $ recur [] $ daily startDate
  where
    startDate = fromJust $ alterYearDay x 1
moments Months x = take days $ recur [] $ daily startDate
  where
    dt = toDateTime x
    days = monthLength (isLeapYear (dtYear dt)) (fromEnum $ dtMonth dt)
    startDate = fromJust $ alterDay x 1
moments Weeks x = take 7 $ recur [] $ daily startDate
  where
    dt = toDateTime x
    delta = fromEnum (dtWeekDay dt) - fromEnum Monday
    startDate = fromJust $ alterYearDay x $ dtYearDay dt - delta
moments _ = mkR

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

limit :: Eq a => [a] -> (Time -> a) -> Moment -> [Moment]
limit xs f m = [m | momentElem m f xs]

byMonth :: Moment a => [Month] -> Frequency -> a -> Recurrence a
byMonth months Years x = Recurrence $ mapMaybe (alterMonth x) months
byMonth months _ x = Recurrence $ [x | dtMonth (toDateTime x) `elem` months]

byWeekNumber :: Moment a => [Int] -> Frequency -> a -> Recurrence a 
byWeekNumber weeks Years x = Recurrence $ mapMaybe (alterWeekNumber x) weeks'
  where
    weeks' = nubSort $ mapMaybe (normIndex 53) weeks
byWeekNumber _ _ = mkR

byYearDay :: Moment a => [Int] -> Frequency -> a -> Recurrence a
byYearDay days freq = go freq days'
  where
    days' = nubSort $ mapMaybe (normIndex 366) days
    go Seconds ds x = Recurrence $ [x | dtYearDay (toDateTime x) `elem` ds]
    go Minutes ds x = Recurrence $ [x | dtYearDay (toDateTime x) `elem` ds]
    go Hours   ds x = Recurrence $ [x | dtYearDay (toDateTime x) `elem` ds]
    go Days    _ = mkR
    go Weeks   _ = mkR
    go Months  _ = mkR
    go Years ds x = Recurrence $ mapMaybe (alterYearDay x) ds

byMonthDay :: Moment a => [Int] -> Frequency -> a -> Recurrence a
byMonthDay days freq = go freq days'
  where
    days' = nubSort $ mapMaybe (normIndex 31) days
    go Seconds ds x = Recurrence $ [x | dtDay (toDateTime x) `elem` ds]
    go Minutes ds x = Recurrence $ [x | dtDay (toDateTime x) `elem` ds]
    go Hours   ds x = Recurrence $ [x | dtDay (toDateTime x) `elem` ds]
    go Days    ds x = Recurrence $ [x | dtDay (toDateTime x) `elem` ds]
    go _       ds x = Recurrence $ mapMaybe (alterDay x) ds

byDay :: Moment a => [WeekDay] -> Frequency -> a -> Recurrence a
byDay days freq = go freq days
  where
   go Seconds ds x = Recurrence $ [x | dtWeekDay (toDateTime x) `elem` ds]
   go Minutes ds x = Recurrence $ [x | dtWeekDay (toDateTime x) `elem` ds]
   go Hours   ds x = Recurrence $ [x | dtWeekDay (toDateTime x) `elem` ds]
   go Days    ds x = Recurrence $ [x | dtWeekDay (toDateTime x) `elem` ds]
   go freq    ds x = filter (onDays ds) $ moments freq x
     where
       onDays ds x = dtWeekDay (toDateTime x) `elem` ds
