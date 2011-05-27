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

We must provide type-qualified versions of the initializers for the
Moment type used (UTCTime)

> dailyUTC :: RecurrenceParameters UTCTime
> dailyUTC = daily
> monthlyUTC :: RecurrenceParameters UTCTime
> monthlyUTC = monthly

> tests :: [Test]
> tests = 
>      [ testGroup "RFC5445 Examples" $ zipWith (testCase . show) [1::Int ..]
>        [ assertEqual ("Test Daily from "++ show date1 ++". 10 Occurrences") 
>            (count 10 $ recur [] dailyUTC{startDate = date1})
>            (count 10 $ recur [byMonthDay [2 .. 11]] monthlyUTC{startDate = date1})
>        , assertEqual ("Test Daily from "++ show date1 ++". Until "++ show date2)
>            (until date2 $ recur [] dailyUTC{startDate = date1})
>            (until date2 $ recur [byDay [Monday .. Sunday]] monthlyUTC{startDate = date1})
>        , assertBool ("Test every other day from "++ show date1 ++ ". Cap at 1000")
>            (checkDayDist 2 $ count 1000 $ recur [] dailyUTC{startDate = date1, interval = toInterval 2})
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
>    go acc x = let d = utcDay x in (abs (diffDays d (snd acc)):(fst acc), d)
>    utcDay (UTCTime d _) = d
> checkDayDist :: Integer -> Recurrence UTCTime -> Bool
> checkDayDist d rs = all (== d) $ dayDist $ fromRecurrence rs
