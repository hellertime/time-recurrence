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

> import System.Locale (defaultTimeLocale, rfc822DateFormat)
> import Data.Time
> import Data.Maybe (fromJust)

> import Prelude hiding (until)
> import Control.Monad ((>=>))
> import Data.Time.Recurrence

We are certain of the validity of the dates used, and so fromJust is safe
to use.

> parse822Time :: String -> UTCTime
> parse822Time = zonedTimeToUTC 
>              . fromJust 
>              . parseTime defaultTimeLocale rfc822DateFormat
> date1 = parse822Time "Tue, 02 Sep 1997 09:00:00 -0400"
> date2 = parse822Time "Wed, 24 Dec 1997 00:00:00 -0400"
> date3 = parse822Time "Thu, 01 Jan 1998 09:00:00 -0400"
> date4 = parse822Time "Mon, 01 Jan 2000 09:00:00 -0400"

> main :: IO ()
> main = defaultMain tests

> until :: (Moment a, Ord a) => a -> [a] -> [a]
> until m = takeWhile (<= m)

> tests :: [Test]
> tests = 
>      [ testGroup "RFC5445 Examples" $ zipWith (testCase . show) [1::Int ..]
>        [ assertEqual ("Test Daily from "++ show date1 ++". 10 Occurrences") 
>            (take 10 $ repeatSchedule' dailyUTC{moment = date1})
>            (take 10 $ repeatSchedule monthlyUTC{moment = date1} $ expand (onMonthDays [2 .. 11]))
>        , assertEqual ("Test Daily from "++ show date1 ++". Until "++ show date2)
>            (until date2 $ repeatSchedule' dailyUTC{moment = date1})
>            (until date2 $ repeatSchedule monthlyUTC{moment = date1} $ expand onEachMonth)
>        , assertBool ("Test every other day from "++ show date1 ++". Cap at 10000")
>            (checkDayDist 2 $ take 10000 $ repeatSchedule' dailyUTC{moment = date1, interval = toInterval 2})
>        , assertEqual ("Test every 10 days from "++ show date1 ++". 5 Occurrences")
>            (take 5 $ repeatSchedule' dailyUTC{moment = date1, interval = toInterval 10})
>            (take 5 $ repeatSchedule yearlyUTC{moment = date1} $ expand (onMonths [September,October]) >=> expand (onMonthDays [2,12,22]))
>        , assertEqual "Test every day in Jan. for 3 years"
>            (until date4 $ repeatSchedule yearlyUTC{moment = date3} $ expand (onMonths [January]) >=> expand onEachMonth)
>            (until date4 $ repeatSchedule dailyUTC{moment = date3} $ restrict (byMonths [January]))
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
