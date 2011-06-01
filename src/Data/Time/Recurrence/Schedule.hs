-- This module is intended to be imported @qualified!, to avoid name
-- clashes with "Prelude" functions. eg.
--
-- > import qualified Data.Time.Recurrence.Schedule as S
module Data.Time.Recurrence.Schedule
    (
      Schedule -- abstract, instances: Eq, Ord, Show
    , toList

      -- * Reducing 'Schedule's (folds)
    , foldl

      -- * Generating 'Schedule's
    , unfoldr
    , iterate

      -- * Transforming 'Schedule's
    , map
    , concatMap

      -- * Searching with a predicate
    , filter
    )
  where

import Prelude hiding (filter, map, foldl, iterate)
import Control.Monad.Reader
import qualified Data.List as L
import Data.List.Ordered (nubSort)
import Data.Monoid
import Data.Time.CalendarTime
import Data.Time.Moment

newtype Schedule a = Schedule { fromSchedule :: [a] } deriving (Show, Eq, Ord)

toList :: Schedule a -> [a]
toList = fromSchedule

instance Monoid (Schedule a) where
  mempty = Schedule []
  x `mappend` y = Schedule $ (fromSchedule x) ++ (fromSchedule y)

-- ----------------------------------------------------------------------------
-- Unfolds

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

foldl :: 
  (a -> a -> Reader (InitialMoment a) a)
  -> a
  -> Schedule a
  -> Reader (InitialMoment a) (Schedule a)
foldl f a s = go a (fromSchedule s)
  where
   go a' []     = return $ Schedule [a']
   go a' (x:xs) = f a' x >>= \a'' -> go a'' xs

filter ::
  (a -> Reader (InitialMoment a) Bool)
  -> Schedule a
  -> Reader (InitialMoment a) (Schedule a)
filter p sch = filterM p (fromSchedule sch) >>= return . Schedule

map ::
  (a -> Reader (InitialMoment a) a)
  -> Schedule a
  -> Reader (InitialMoment a) (Schedule a)
map f s = mapM f (fromSchedule s) >>= return . Schedule

concatMap ::
  (a -> Reader (InitialMoment a) (Schedule a))
  -> Schedule a
  -> Reader (InitialMoment a) (Schedule a)
concatMap f s = mapM f (fromSchedule s) >>= return . mconcat
