module Data.Time.Recurrence
    (
      -- * The @WeekDay@ type
      WeekDay (..)

      -- * The @Month@ type
    , Month (..)

      -- * The @Moment@ type
    , Moment (..)

      -- * @Recurrence@ type rule effects
    , byMonth
    )
  where

import Data.List.Ordered (nub, nubSort)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Time
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate, toOrdinalDate, fromOrdinalDateValid)
import Data.Time.Calendar.WeekDate (fromWeekDate, fromWeekDateValid, toWeekDate)

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
momentElem m field xs = (field $ utcToTime $ moment m) `elem` xs

momentChangeWeekNumber :: Moment -> Int -> Maybe Moment
momentChangeWeekNumber m w = do
  let (UTCTime d t) = moment m
  let tm = utcToTime $ moment m
  d' <- fromWeekDateValid (year tm) w (fromEnum $ weekDay tm)
  return $ m{moment = (UTCTime d' t)}

momentChangeYearDay :: Moment -> Int -> Maybe Moment
momentChangeYearDay m yd = do
  let (UTCTime d t) = moment m
  let tm = utcToTime $ moment m
  d' <- fromOrdinalDateValid (year tm) yd
  return $ m{moment = (UTCTime d' t)}

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
                                     (toEnum dow)
  where
    (y, m, d) = toGregorian utcDay
    (TimeOfDay hh mm ss) = timeToTimeOfDay utcTime
    yDay = snd $ toOrdinalDate utcDay
    (_, _, dow) = toWeekDate utcDay

-- | Convert the @Time@ components into a @UTCTime@
timeToUTC :: Time -> UTCTime
timeToUTC tm = UTCTime d t
  where
    d = fromGregorian (year tm) (fromEnum $ month tm) (day tm)
    t = timeOfDayToTime $ TimeOfDay (hour tm) (minute tm) (toEnum $ second tm)

-- | Construct a @UTCTime@ at midnight
utcGregorian :: Integer -> Int -> Int -> UTCTime
utcGregorian y m d = UTCTime (fromGregorian y m d) (timeOfDayToTime midnight)

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

addMonthsClip :: Integer -> UTCTime -> UTCTime
addMonthsClip = addUTCDays addGregorianMonthsClip

addMonthsRollOver :: Integer -> UTCTime -> UTCTime
addMonthsRollOver = addUTCDays addGregorianMonthsRollOver

addMonths :: Bool -> Integer -> UTCTime -> UTCTime
addMonths b = if b then addMonthsRollOver else addMonthsClip

addYearsClip :: Integer -> UTCTime -> UTCTime
addYearsClip = addUTCDays addGregorianYearsClip

addYearsRollOver :: Integer -> UTCTime -> UTCTime
addYearsRollOver = addUTCDays addGregorianYearsRollOver

addYears :: Bool -> Integer -> UTCTime -> UTCTime
addYears b = if b then addYearsRollOver else addYearsClip

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
    scale = scaleUTCTime addTime 

{-
bySetPosition :: [Int] -> Recurrence -> Recurrence
bySetPosition = go
  where
    go :: [Int] -> Recurrence -> Recurrence
    go [] r = r
    go ns r = 
      let ms = moments r 
      in BySetPosition $ map (ms !!) (nubSort $ norm ns (length ms))
    norm ns l     = foldl (f l) [] ns
    f l acc 0 = acc
    f l acc n =
      case n > 0 of
        True -> if n - 1 < l then (n-1):acc else acc
        False -> 
          let n' = l+n in
            if n' > 0 && n' < l then (-n):acc else acc
-}

-- | Normalize an bounded index
normIndex :: Int -> Int -> Maybe Int
normIndex max 0 = Nothing
normIndex max idx =
  if (abs idx) > max
    then Nothing
    else Just $ (idx + max') `mod` max'
  where
    max' = max + 1

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
byWeekNumber weeks m = [m]

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
    days' = nubSort $ mayMaybe (normIndex 31) days
    go days m@(Weekly _) = [m]
    go days m@(Secondly _) = limit days day m
    go days m@(Minutely _) = limit days day m
    go days m@(Hourly _)   = limit days day m
    go days m@(Daily _)    = limit days day m
    go days m  = map (setDay m) days
    setDay :: Moment -> Int -> Moment
    setDay m d = m{moment = timeToUTC (utcToTime $ moment m){day = d}}

byDay :: [WeekDay] -> Moment -> [Moment]
