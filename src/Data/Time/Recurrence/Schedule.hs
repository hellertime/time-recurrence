module Data.Time.Recurrence.Schedule
    (
      Schedule (fromSchedule)
    )
  where

import Control.Monad.Reader
import Data.Foldable
import qualified Data.List as L
import Data.Monoid
import Data.Time.Moment

newtype Schedule a = Schedule { fromSchedule :: [a] }

instance Monoid (Schedule a) where
  mempty = Schedule []
  x `mappend` y = Schedule $ (fromSchedule x) ++ (fromSchedule y)

instance Functor Schedule where
  fmap f = Schedule . map f . fromSchedule

instance Foldable Schedule where
  foldr f b sch = (L.foldr f b $ fromSchedule sch)

-- | The 'unfoldr' function is analogous to the List 'unfoldr'. 
-- 'unfoldr' builds a 'Schedule' from a seed value. The function takes the 
-- element and returns 'Nothing' if it is done producing the 'Schedule', 
-- otherwise 'Just' @(a, b)@. In this case, @a@ is a 'Schedule' to which 
-- further output is appended to and b is used as the next seed in a 
-- recursive call
unfoldr :: 
  (a -> Reader (InitialMoment a) (Maybe (Schedule a, a))) -- ^ builder function
  -> a -- ^ seed value
  -> Reader (InitialMoment a) (Schedule a) -- ^ the resulting schedule
unfoldr f b = do
  r <- f b
  case r of
    Just (a, new_b) -> unfoldr f new_b 
                       >>= \b -> return $ a `mappend` b
    Nothing         -> return mempty

-- | 'iterate' returns an ininite 'Schedule' of repeated applications of 'f' on
-- the 'Reader'
iterate ::
  (a -> Reader (InitialMoment a) a)
  -> Reader (InitialMoment a) (Schedule a)
iterate f = asks moment 
            >>= unfoldr (\x -> f x 
                               >>= \x' -> return $ Just (Schedule [x'], x'))
