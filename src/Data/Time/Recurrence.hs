module Data.Time.Recurrence
    (
    )
  where

-- | The base frequency of a Recurrence.
--
-- The frequencies are chosen fromRFC 5545.
data Frequencey =
      Secondly    -- ^ every second
    | Minutely    -- ^ every minute
    | Hourly      -- ^ every hour
    | Daily       -- ^ every day
    | Weekly      -- ^ every week
    | Monthly     -- ^ every month
    | Yearly      -- ^ every year
  deriving (Show, Eq)
