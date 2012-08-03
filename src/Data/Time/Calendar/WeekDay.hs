module Data.Time.Calendar.WeekDay
    (
      -- * WeekDay
      WeekDay(..)
    )
  where

data WeekDay 
    = Monday 
    | Tuesday 
    | Wednesday 
    | Thursday 
    | Friday 
    | Saturday 
    | Sunday
  deriving (Read, Show, Eq, Ord, Enum, Bounded)
