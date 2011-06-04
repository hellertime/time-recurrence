module Data.Time.Recurrence.Recurrence
    (
{-
      repeatSchedule
    , repeatSchedule1
    , by
    , by'
    , calendarDay'
    , calendarYearDay'
-}
    )
  where

import Control.Monad.Reader
import Data.List.Ordered (nubSort)
import Data.Maybe (mapMaybe)
import Data.Time.CalendarTime
import Data.Time.Moment
import qualified Data.Time.Recurrence.Schedule as S

{-
enumMoments ::
  (Interval -> Frequency -> a -> a)
  -> Reader (InitialMoment a) (S.Schedule a)
enumMoments step = S.iterate $ \m -> do
  i <- ask
  return $ step (interval i) (frequency i) m

-- | 'enumFutureMoments' is a 'Schedule' of all future moments derived 
-- from the 'InitialMoment'
enumFutureMoments :: Moment a => Reader (InitialMoment a) (S.Schedule a)
enumFutureMoments = enumMoments next

-- | 'repeatSchedule' runs a schedule
repeatSchedule :: 
  Moment a => 
  InitialMoment a 
  -> (S.Schedule a -> Reader (InitialMoment a) (S.Schedule a))
  -> [a]
repeatSchedule init r = S.toList $ runReader (enumFutureMoments >>= r) init

-- | 'repeatSchedule1' is a schedule without rules
repeatSchedule1 :: Moment a => InitialMoment a -> [a]
repeatSchedule1 init = repeatSchedule init return

newtype CalendarTimeOrdinalAccessor = CTOA { unCTOA :: (CalendarTime -> Int, Int) }

-- | Normalize an bounded ordinal index
--   Pass an upper-bound 'ub' and an index 'idx'
--   Converts 'idx' < 0 into valid 'idx' > 0 or
--   Nothing
normalizeOrdinalIndex :: Int -> Int -> Maybe Int
normalizeOrdinalIndex _ 0 = Nothing
normalizeOrdinalIndex ub idx =
  if abs idx > ub
    then Nothing
    else Just $ (idx + ub') `mod` ub'
  where
    ub' = ub + 1

-- | Predicate builder for 'filter'
by :: 
  (CalendarTimeConvertible a, Ord b) =>
  (CalendarTime -> b)
  -> [b]
  -> a
  -> Reader (InitialMoment a) Bool
by f bs a = return $ f (toCalendarTime a) `elem` nubSort bs

by' ::
  (CalendarTimeConvertible a) =>
  CalendarTimeOrdinalAccessor
  -> [Int]
  -> a
  -> Reader (InitialMoment a) Bool
by' ctoa bs = let (f, n) = unCTOA ctoa 
              in by f $ mapMaybe (normalizeOrdinalIndex n) bs

calendarDay' :: CalendarTimeOrdinalAccessor
calendarDay' = CTOA (calendarDay, 31)

calendarYearDay' :: CalendarTimeOrdinalAccessor
calendarYearDay' = CTOA (calendarYearDay, 366)

newtype MomentOrdinalMutator a = MOM 
                             { unMOM :: (a -> Int -> Maybe a, Int) }

-- | Schedule builder for 'concatMap'
on ::
  (CalendarTimeConvertible a, Moment a) =>
  (a -> b -> Maybe a)
  -> [b]
  -> a
  -> Reader (InitialMoment a) (Schedule a)
on f bs a = return $ Schedule $ mapMaybe (f a) bs
-}

