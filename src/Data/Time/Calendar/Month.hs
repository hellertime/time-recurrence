module Data.Time.Calendar.Month
    (
      -- * Month
      Month (..)
    ) 
  where

data Month
    = January
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
  fromEnum June	     = 6
  fromEnum July	     = 7
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
