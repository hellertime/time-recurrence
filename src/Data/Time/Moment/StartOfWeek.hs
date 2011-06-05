module Data.Time.Moment.StartOfWeek
    (
      -- * StartOfWeek
      StartOfWeek (fromStartOfWeek)
    , toStartOfWeek
    )
  where

import Data.Time.Calendar.WeekDay
import Data.Time.Moment.Private

toStartOfWeek :: WeekDay -> StartOfWeek
toStartOfWeek = StartOfWeek
