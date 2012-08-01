{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Data.Time.Recurrence.AndThen
    (
      AndThen (..)
    )
  where

infixr 0 >==>

class AndThen a b c | a b -> c where
  (>==>) :: a -> b -> c
