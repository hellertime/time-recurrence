module Data.Time.Moment.Private
    (
      -- * Interval
      Interval (..)
     
      -- * StartOfWeek
    , StartOfWeek (..)
    )
  where

import Data.Time.Calendar.WeekDay

newtype Interval = Interval { fromInterval :: Integer } deriving (Show)
newtype StartOfWeek = StartOfWeek { fromStartOfWeek :: WeekDay } deriving (Show)
