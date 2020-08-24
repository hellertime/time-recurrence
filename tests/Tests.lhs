> {-# LANGUAGE CPP #-}

HUnut test suite of example recurrences lifted from RFC 5545 section 3.8.5.3

> module Main where

> import Test.Framework (Test, defaultMain, testGroup)
> import Test.Framework.Providers.HUnit
> import Test.HUnit hiding (Test)

There are various dates used during these tests, some as start dates and
some as end dates.

The examples are actually all in the America/New_York time zone, but since
a local time instance has not been created yet, all the dates are converted
into UTC.

#if MIN_VERSION_time(1,5,0)
> import Data.Time hiding (DayOfWeek( Monday ), DayOfWeek( Sunday ))
#else
> import Data.Time
> import System.Locale (TimeLocale, defaultTimeLocale, rfc822DateFormat)
#endif

> import Data.Maybe (fromJust)
> import Prelude hiding (until, filter)
> import Data.Time.Recurrence

> timeParse :: ParseTime t => TimeLocale -> String -> String -> Maybe t
#if MIN_VERSION_time(1,5,0)
> timeParse = parseTimeM True
#else
> timeParse = parseTime
#endif

We are certain of the validity of the dates used, and so fromJust is safe
to use.

> date1, date2, date3, date4 :: UTCTime
> parse822Time :: String -> UTCTime
> parse822Time = zonedTimeToUTC
>              . fromJust
>              . timeParse defaultTimeLocale rfc822DateFormat
> date1 = parse822Time "Tue, 02 Sep 1997 09:00:00 -0400"
> date2 = parse822Time "Wed, 24 Dec 1997 00:00:00 -0400"
> date3 = parse822Time "Thu, 01 Jan 1998 09:00:00 -0400"
> date4 = parse822Time "Mon, 31 Jan 2000 09:00:00 -0400"

> main :: IO ()
> main = defaultMain tests

> until :: (Moment a, Ord a) => a -> [a] -> [a]
> until m = takeWhile (<= m)

> tests :: [Test]
> tests =
>      [ testGroup "RFC5445 Examples" $ zipWith (testCase . show) [1::Int ..]
>        [ assertEqual ("Test Daily from "++ show date1 ++". 10 Occurrences")
>            (take 10 $ starting date1 $ recur daily)
>            (take 10 $ starting date1 $ recur monthly >==> enum (Days [2 .. 11]))
>        , assertEqual ("Test Daily from "++ show date1 ++". Until "++ show date2)
>            (until date2 $ starting date1 $ recur daily)
>            (until date2 $ starting date1 $ recur monthly >==> enum (WeekDaysInMonth [Monday .. Sunday]))
>        , assertBool ("Test every other day from "++ show date1 ++". Cap at 10000")
>            (checkDayDist 2 $ take 10000 $ starting date1 $ recur $ daily `by` 2)
>        , assertEqual ("Test every 10 days from "++ show date1 ++". 5 Occurrences")
>            (take 5 $ starting date1 $ recur $ daily `by` 10)
>            (take 5 $ starting date1 $ recur yearly >==> enum (Months [September, October]) >==> enum (Days [2,12,22]))
>        , assertEqual "Test every day in Jan. for 3 years"
>            (until date4 $ starting date3 $ recur yearly >==> enum (Months [January]) >==> enum (WeekDaysInMonth [Monday .. Sunday]))
>            (until date4 $ starting date3 $ recur daily >==> filter (Months [January]))
>        ]
>      , testGroup "Tests to ensure Github Issue-1 is fixed" $ zipWith (testCase . show) [1::Int ..]
>        [ getCurrentTime >>= \now ->
>          assertEqual ("Generate 3 dates on the 0 and 30 minute marks")
>            3 (length $ take 3 . starting now $ recur daily >==> enum (Minutes [0,30]))
>        , getCurrentTime >>= \now ->
>          assertEqual ("Generate a single date on the 0 minute mark")
>           1 (length $ take 1 . starting now $ recur daily >==> enum (Minutes [0]))
>        ]
>      ]

This is the assertion function for testing the number of days between moments.
It will be used in a couple of tests, and requires at least two moments to
operate correctly.

> dayDist :: [UTCTime] -> [Integer]
> dayDist [] = []
> dayDist (_:[]) = []
> dayDist (x:xs) = fst $ foldl go ([], utcDay x) xs
>  where
>    go acc x = let d = utcDay x in (abs (diffDays d (snd acc)):fst acc, d)
>    utcDay (UTCTime d _) = d
> checkDayDist :: Integer -> [UTCTime] -> Bool
> checkDayDist d = all (== d) . dayDist
