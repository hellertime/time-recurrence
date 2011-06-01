module Data.Time.Recurrence.Private
    (
      concatMapM
    )
  where

import Control.Applicative

concatMapM :: Applicative m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> traverse f xs
