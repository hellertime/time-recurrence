-- This module is intended to be imported @qualified!, to avoid name
-- clashes with "Prelude" functions. eg.
--
-- > import qualified Data.Time.Recurrence.Schedule as S
module Data.Time.Recurrence.Schedule
    (
      Schedule -- abstract, instances: Eq, Ord, Show
    , recur
    , starting
    , begin
{-
    , every
    , onMonths
    , only
    , byWeekDays
-}
    )
  where

import Control.Monad.Reader
import Data.List as L
import Data.List.Ordered as O
import Data.Maybe (fromJust, fromMaybe)
import Data.Time.Calendar.Month
import Data.Time.Calendar.WeekDay
import Data.Time.CalendarTime hiding (withDay)
import Data.Time.Moment

type Schedule a = Reader (InitialMoment a) [a]

runSchedule :: Schedule a -> InitialMoment a -> [a]
runSchedule = runReader

repeatSchedule :: 
  Moment a  => 
  InitialMoment a 
  -> (Schedule a -> Schedule a) 
  -> [a]
repeatSchedule im sch = runSchedule (sch iterateInitialMoment) im
  where
    iterateInitialMoment :: Moment a => Schedule a
    iterateInitialMoment = do
      im <- ask
      return $ iterate (next (interval im) (period im)) (moment im)

recur :: a -> a
recur = id

-- | 'starting' is an infinite list of 'Moment's, where no 'Moment' 
-- occurrs before the 'InitialMoment'. The list is further refined
-- by the passed in function.
starting :: 
  (Ord a, Moment a) => 
  InitialMoment a 
  -> a 
  -> (Schedule a -> Schedule a) 
  -> [a]
starting im m0 = dropWhile (< m0) . O.nub . repeatSchedule im{moment=m0}

begin :: 
  (Ord a, Moment a) => 
  InitialMoment a 
  -> a 
  -> [a]
begin im m0 = starting im m0 (liftM id)



{-
-- | 'onMonths' computes the bounds for the 'month' given a 'moment', and
-- returns a starting value and function to generate the moments with 'unfoldr'
onMonths :: 
  (CalendarTimeConvertible a, Moment a, Ord a) => 
  Month 
  -> (a -> Maybe (a, a))
onMonths months = go months
  go []     _  = Nothing
  go (m:ms) m0 = do
    m0    <- withDay moment 1
    start <- withMonth m0 month
    end   <- withDay start $ lastDayOfMonth start
  return (start, \a0 -> if a0 <= end then Just (a0, step a0) else Nothing)
  where step = next (toInterval 1) Days

-- | 'every' maps over the 'Moment's in the 'Schedule' and for each
-- 'Moment' it applys an unfold using the supplied function.
-- The associated list 
every ::
  Moment a => 
  (a -> b -> (a, a -> Maybe (a, a))) 
  -> [b] 
  -> [a] 
  -> Schedule a
every f bs as = return $ concat [(uncurry $ flip L.unfoldr) (f a b) 
                                | b <- bs
                                , a <- as]

byWeekDays :: (CalendarTimeConvertible a, Moment a) => [WeekDay] -> a -> Bool
byWeekDays bs = flip elem bs . calendarWeekDay . toCalendarTime

only :: Moment a => ([b] -> a -> Bool) -> [b] -> [a] -> Schedule a
only f bs as = return $ filter (f bs) as
-}
