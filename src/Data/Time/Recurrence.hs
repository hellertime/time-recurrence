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


-- | Symbolic week days.
--
-- Note: The first Day of the Week is Monday 
-- TODO: Move this to a more general library
data WeekDay =
      Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
  deriving (Show, Eq, Ord, Enum, Bounded)


-- | Symbolic months.
--
-- TODO: Move this to a more general library
data Month
      January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December
  deriving (Show, Eq, Ord, Bounded)

instance Enum Month where
  fromEnum January   = 1
  fromEnum February  = 2
  fromEnum March     = 3
  fromEnum April     = 4
  fromEnum May       = 5
  fromEnum June      = 6
  fromEnum July      = 7
  fromEnum August    = 8
  fromEnum September = 9
  fromEnum October   = 10
  fromEnum November  = 11
  fromEnum December  = 12

  toEnum 1  = January
  toEnum 2  = February
  toEnum 3  = March
  toEnum 4  = April
  toEnum 5  = May
  toEnum 6  = June
  toEnum 7  = July
  toEnum 8  = August
  toEnum 9  = September
  toEnum 10 = October
  toEnum 11 = November
  toEnum 12 = December

  toEnum unmatched = error ("Month.toEnum: Cannot match " ++ show unmatched)


type Refinement = (Eq a) => Maybe ((Moment -> a) -> [a] -> Recurrence -> Maybe Recurrence)

-- | Recurrence data type
--
-- This encapulates a point-in-time of a recurring series
-- from here we can compute the 'next' and 'prev' Recurrence
data Recurrence = R
    { pointInTime  :: UTCTime    -- ^ the current moment
    , startOfWeek  :: WeekDay    -- ^ used in calculations on a week
                                 --   TODO: compute accurately when this
                                 --         is another value than Monday
                                 -- ^ (RFC 5545 defaults to Monday)
    , frequency    :: Frequency  -- ^ defines the base frequency
    , interval     :: Integer    -- ^ defines the step size 
    , rollOver     :: Bool       -- ^ for values that would exceed the base
                                 --   frequency, we clip them to the freq
                                 --   unless this is True
    , refinement   :: Refinement -- ^ A possible function which will modify
                                 --   the recurrence pattern from the default
                                 --   frequency
    }
  deriving (Show)

instance Eq Recurrence where
  x == y = (pointInTime x) == (pointInTime y)
