module Data.Time.Moment.UTC
    (
    )
  where

import Data.Time
import Data.Time.Moment.Moment

instance Moment UTCTime where
  epoch = UTCTime (toEnum 0) 0
  addSeconds utc i = addUTCTime (fromIntegral i) utc
  addMonths (UTCTime d t) i = UTCTime (addGregorianMonthsRollOver i d) t
  addYears (UTCTime d t) i = UTCTime (addGregorianYearsRollOver i d) t
