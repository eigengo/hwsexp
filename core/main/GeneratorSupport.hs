module GeneratorSupport(fromRange, GeneratorDelay, GeneratorFunction) where

import Syntax
import System.Random (randomIO)
import Control.Applicative ((<$>))

-- |Function that delays the generator for the given number of microseconds.
--  A suitable value is @threadDelay@
type GeneratorDelay = Int -> IO ()

-- |Function that can be applied to some callback to generate the values
type GeneratorFunction a b = (([a] -> IO b) -> IO b)

-- |Picks one random value from the given range
fromRange :: Range -> IO Int
fromRange (Exact x) = return x
fromRange (Between l u) = ((l +) . (`mod` (u + 1 - l)) <$> (randomIO :: IO Int))
