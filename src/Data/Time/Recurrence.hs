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

-- | @Moment@ data type
--   One per Frequency (Is this the best way to define this?)
data Moment
    = Secondly { moment :: UTCTime }
    | Minutely { moment :: UTCTime }
    | Hourly   { moment :: UTCTime }
    | Daily    { moment :: UTCTime }
    | Weekly   { moment :: UTCTime }
    | Monthly  { moment :: UTCTime }
    | Yearly   { moment :: UTCTime }
  deriving (Show, Eq, Ord)

-- | Test if (field t) is elem in xs
momentElem :: Eq a => Moment -> (Time -> a) -> [a] -> Bool
momentElem m field xs = field (utcToTime $ moment m) `elem` xs

momentChangeWeekNumber :: Moment -> Int -> Maybe Moment
momentChangeWeekNumber m w = do
  let (UTCTime _ t) = moment m
  let tm = utcToTime $ moment m
  d <- fromMondayStartWeekValid (year tm) w (fromEnum $ weekDay tm)
  return $ m{moment = UTCTime d t}

momentChangeYearDay :: Moment -> Int -> Maybe Moment
momentChangeYearDay m yd = do
  let (UTCTime _ t) = moment m
  let tm = utcToTime $ moment m
  d <- fromOrdinalDateValid (year tm) yd
  return $ m{moment = UTCTime d t}

-- | @Time@ data type
data Time = T
    { year       :: Integer
    , month      :: Month
    , day        :: Int
    , hour       :: Int
    , minute     :: Int
    , second     :: Int
    , yearDay    :: Int
    , weekDay    :: WeekDay
    }
  deriving (Show)

-- | Expand a @UTCTime@ into its @Time@ components
utcToTime :: UTCTime -> Time
utcToTime (UTCTime utcDay utcTime) = T y (toEnum m) d
                                     hh mm (fromEnum ss)
                                     yDay
                                     (toEnum $ dow - 1)
  where
    (y, m, d) = toGregorian utcDay
    (TimeOfDay hh mm ss) = timeToTimeOfDay utcTime
    yDay = snd $ toOrdinalDate utcDay
    (_, dow) = mondayStartWeek utcDay

-- | Convert the @Time@ components into a @UTCTime@
timeToUTC :: Time -> UTCTime
timeToUTC tm = UTCTime d t
  where
    d = fromGregorian (year tm) (fromEnum $ month tm) (day tm)
    t = timeOfDayToTime $ TimeOfDay (hour tm) (minute tm) (toEnum $ second tm)

-- | Construct a @UTCTime@ at midnight
utcGregorian :: Integer -> Int -> Int -> UTCTime
utcGregorian y m d = UTCTime (fromGregorian y m d) (timeOfDayToTime midnight)

-- | Construct a @UTCTime@ at a time
utcGregorianWithTime :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
utcGregorianWithTime y m d hh mm ss = UTCTime d' t'
  where
    d' = fromGregorian y m d
    t' = timeOfDayToTime (TimeOfDay hh mm (toEnum ss))

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

-- UTCTime addition
addTime :: Integer -> UTCTime -> UTCTime
addTime i = addUTCTime (fromIntegral i)

addUTCDays :: (Integer -> Day -> Day) -> Integer -> UTCTime -> UTCTime
addUTCDays f i (UTCTime d t) = UTCTime (f i d) t

addMonthsRollOver :: Integer -> UTCTime -> UTCTime
addMonthsRollOver = addUTCDays addGregorianMonthsRollOver

addYearsRollOver :: Integer -> UTCTime -> UTCTime
addYearsRollOver = addUTCDays addGregorianYearsRollOver

-- | Moment addition
scaleUTCTime :: (Integer -> UTCTime -> UTCTime) -> Integer -> Moment -> Moment 
scaleUTCTime f s m = m{moment = f s (moment m)}

-- | Increment a Moment to its next value
next :: Integer -> Moment -> Moment
next interval = go
  where
    go m@(Secondly _) = scale oneSecond m
    go m@(Minutely _) = scale oneMinute m
    go m@(Hourly _)   = scale oneHour m
    go m@(Daily _)    = scale oneDay m
    go m@(Weekly _)   = scale oneWeek m
    go m@(Monthly _)  = scaleUTCTime addMonthsRollOver interval m
    go m@(Yearly _)   = scaleUTCTime addYearsRollOver interval m
    scale x = scaleUTCTime addTime $ interval * x

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
moments :: Moment -> [Moment]
moments m@(Yearly u) = 
  if isLeapYear (year $ utcToTime u)
    then map yearly $ take 366 $ recur [] startDate'
    else map yearly $ take 365 $ recur [] startDate'
  where
    yearly = Yearly . moment 
    startDate = fromJust $ momentChangeYearDay m 1
    startDate' = Daily $ moment startDate
moments (Monthly u) = map monthly $ take days $ recur [] startDate
  where
    monthly = Monthly . moment
    tm = utcToTime u
    startDate = Daily $ timeToUTC tm{day = 1}
    days = monthLength (isLeapYear (year tm)) (fromEnum $ month tm)
moments m@(Weekly u) = map weekly $ take 7 $ recur [] $ Daily (moment m')
  where
    weekly = Weekly . moment
    tm = utcToTime u
    delta = fromEnum (weekDay tm) - fromEnum Monday
    m' = fromJust $ momentChangeYearDay m (yearDay tm - delta)
moments m = [m]

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

byMonth :: [Month] -> Moment -> [Moment]
byMonth months m@(Yearly _) = map (setMonth m) months
  where
    setMonth :: Moment -> Month -> Moment
    setMonth m mo = m{moment = timeToUTC (utcToTime $ moment m){month = mo}}
byMonth months m = limit months month m

byWeekNumber :: [Int] -> Moment -> [Moment]
byWeekNumber weeks m@(Yearly _) = mapMaybe (momentChangeWeekNumber m) weeks'
  where
    weeks' = nubSort $ mapMaybe (normIndex 53) weeks
byWeekNumber _ m = [m]

byYearDay :: [Int] -> Moment -> [Moment]
byYearDay days = go days'
  where
    days' = nubSort $ mapMaybe (normIndex 366) days
    go days m@(Secondly _) = limit days yearDay m
    go days m@(Minutely _) = limit days yearDay m
    go days m@(Hourly _)   = limit days yearDay m
    go _ m@(Daily _)   = [m]
    go _ m@(Weekly _)  = [m]
    go _ m@(Monthly _) = [m]
    go days m@(Yearly _) = mapMaybe (momentChangeYearDay m) days

byMonthDay :: [Int] -> Moment -> [Moment]
byMonthDay days = go days'
  where
    days' = nubSort $ mapMaybe (normIndex 31) days
    go _ m@(Weekly _) = [m]
    go days m@(Secondly _) = limit days day m
    go days m@(Minutely _) = limit days day m
    go days m@(Hourly _)   = limit days day m
    go days m@(Daily _)    = limit days day m
    go days m = map (setDay m) days
    setDay :: Moment -> Int -> Moment
    setDay m d = m{moment = timeToUTC (utcToTime $ moment m){day = d}}

byDay :: [WeekDay] -> Moment -> [Moment]
byDay days = go (nubSort days)
  where
    go days m@(Secondly _) = limit days weekDay m
    go days m@(Minutely _) = limit days weekDay m
    go days m@(Hourly _)   = limit days weekDay m
    go days m@(Daily _)    = limit days weekDay m
    go days m = filter (\x -> momentElem x weekDay days) $ moments m
    
