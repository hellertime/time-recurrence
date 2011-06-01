module Data.Time.Recurrence.Schedule
    (
      Schedule (fromSchedule)
    )
  where

import Data.Foldable
import Data.Monoid
import Data.Time.Moment

newtype Schedule a = Schedule { fromSchedule :: [a] }

instance Monoid (Schedule a) where
  mempty = Schedule []
  x `mappend` y = Schedule $ (fromSchedule x) `mappend` (fromSchedule y)

instance Functor Schedule where
  fmap f = Schedule . map f . fromSchedule

instance Foldable Schedule where
  foldr f = Schedule . foldr f . fromSchedule

-- | /O(n)/, where @n@ is the length of the result. The 'unfoldr' function 
-- is analogous to the List 'unfoldr'. 'unfoldr' builds a 'Schedule' from a 
-- seed value. The function takes the element and returns 'Nothing' if it is
-- done producing the 'Schedule', otherwise 'Just' @(a, b)@. In this case, @a@
-- is a 'Schedule' to which further output is appended to and b is used as the
-- next seed in a recursive call
unfoldr :: 
  (a -> Reader (InitialMoment a) (Maybe (Schedule a, a))) -- ^ builder function
  -> a -- ^ seed value
  -> Reader (InitialMoment a) (Schedule a) -- ^ the resulting schedule
unfoldr f = unfoldr' mempty
  where
    unfoldr' sch a = (f a) >>= return . maybe sch unfoldr'')
      where unfoldr'' sch' a' = unfoldr' (sch `mappend` sch') a'
