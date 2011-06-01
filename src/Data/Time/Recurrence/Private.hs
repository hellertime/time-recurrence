module Data.Time.Recurrence.Private
    (
      concatMapM
    , mapNormIndex
    )
  where

import Control.Applicative
import Data.Traversable
import Data.Maybe (mapMaybe)

concatMapM :: Applicative m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> traverse f xs

-- | Normalize an bounded index
--   Pass an upper-bound 'ub' and an index 'idx'
--   Converts 'idx' < 0 into valid 'idx' > 0 or
--   Nothing
normIndex :: Int -> Int -> Maybe Int
normIndex _ 0 = Nothing
normIndex ub idx =
  if abs idx > ub
    then Nothing
    else Just $ (idx + ub') `mod` ub'
  where
    ub' = ub + 1

mapNormIndex :: Int -> [Int] -> [Int]
mapNormIndex n = mapMaybe (normIndex n)


