HUnut test suite of example recurrences lifted from RFC 5545 section 3.8.5.3

> module Main where

> import Test.Framework (Test)
> import Test.Framework (defaultMain, testGroup)
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

> tests :: [Test]
> tests = 
>      [ testGroup "RFC5445 Examples" $ zipWith (testCase . show) [1::Int ..] $
>        [ assertEqual "Test Daily. 10 Occurrences" 
>            (count 10 $ recur [] daily date1)
>            (count 10 $ recur [byMonthDay [2..11]] monthly date1)
>        ]
>      ]
