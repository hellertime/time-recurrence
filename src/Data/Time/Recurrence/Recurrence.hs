module Data.Time.Recurrence.Recurrence
    (
      repeatSchedule
    , repeatSchedule1
    )
  where

import Control.Monad.Reader
import Data.Time.Moment
import qualified Data.Time.Recurrence.Schedule as S

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
