module Data.Time.Moment.Interval
    (
      -- * Interval
      Interval (fromInterval)
    , toInterval
    )
  where

import Data.Time.Moment.Private

toInterval :: Integer -> Interval
toInterval = Interval
