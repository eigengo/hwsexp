module GeneratorSupport(fromRange, GeneratorDelay, GeneratorCallback) where

import Syntax
import System.Random (randomIO)
import Control.Applicative ((<$>))

-- |Function that delays the generator for the given number of microseconds.
--  A suitable value is @threadDelay@
type GeneratorDelay = Int -> IO ()

-- |Function that can be passed to the generator that is called in every step
type GeneratorCallback a = [Int] -> IO a

-- |Picks one random value from the given range
fromRange :: Range -> IO Int
fromRange (Exact x) = return x
fromRange (Between l u) = ((l +) . (`mod` (u + 1 - l)) <$> (randomIO :: IO Int))
