module Data.Time.Moment.UTC
    (
      secondlyUTC
    , minutelyUTC
    , hourlyUTC
    , dailyUTC
    , weeklyUTC
    , monthlyUTC
    , yearlyUTC
    )
  where

import Data.Time
import Data.Time.Moment.Moment

instance Moment UTCTime where
  epoch = UTCTime (toEnum 0) 0
  addSeconds utc i = addUTCTime (fromIntegral i) utc
  addMonths (UTCTime d t) i = UTCTime (addGregorianMonthsRollOver i d) t
  addYears (UTCTime d t) i = UTCTime (addGregorianYearsRollOver i d) t

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
