module Utilities
( (|>)
, maxBy
, bytesToString
) where

import Data.Ord
import Data.Foldable
import Data.Char
import Control.Monad

(|>) = flip (.)

maxBy :: Ord b => Foldable f => (a -> b) -> f a -> a
maxBy = maximumBy . comparing

bytesToString :: Integral i => Monad m => m i -> m Char
bytesToString = liftM (chr . fromIntegral)  