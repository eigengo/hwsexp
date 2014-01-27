{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Generator(generator, Generator(..), GeneratorDelay) where

import Syntax
import GeneratorSupport
import Text.Parsec.Error

import Control.Monad (forever)
import Control.Applicative ((<$>))

import qualified Parser as P
import qualified Standard.Parser as SP
import qualified Custom.Parser as CP

import qualified Standard.Generator as SG
import qualified Custom.Generator as CG

-- |Generator is a type that performs a step of the generation process
newtype Generator a b = Generator { 

  -- |Performs the generator step
  runGenerator :: GeneratorDelay     -- ^The delaying function. You can pass in @threadDelay@, for example.
               -> (a -> IO b)        -- ^The operation to execute in every step
               -> IO b               -- ^The final result
}

-- evendistr 10 [0..100] 
-- evendistr 10 [0..100] once
-- evendistr 10 [0..100] 5 times
-- evendistr 10 [0..100] [0..5] times
-- evendistr 10 [0..100] forever 
-- evendistr 10 [0..100] ... every 1000ms
-- evendistr 10 [0..100] ... every 1000ms
-- evendistr 10 [0..100] ... every [1000..2000]ms
-- evendistr [10..20] ...

-- |Prepares a generator by parsing the input string. It returns a value that
--  you can use to get the generated values.
--   
--  Typically, you'd have code similar to
--  @
--      case (generator "evendistr 5 [1..20] every 50ms") of
--          Left err  -> ...                                         -- what do you want, a cookie?
--          Right gen -> runGenerator gen threadDelay (\nums -> ...) -- good user, have numbers
-- @
-- In GHCi, you can try out
-- @
--     let (Right nums) = (runGenerator) <$> generator ("evendistr 5 [1..20] 1 times every 50ms")
--     nums (const $ return ()) return
--     -- or, if you have import Control.Concurrent (threadDelay)
--     nums threadDelay return
-- @
generator :: String                             -- ^The expression to parse
          -> Either ParseError (Generator [Int] b)  -- ^The result with errors or the ready Generator
generator input = do
  (Expression exp rep del) <- P.parseExpression input
  -- not yet used pe
  gen <- case exp of
           Standard body -> SG.mkGenerator <$> SP.parseToplevel body 
           Custom body   -> CG.mkGenerator <$> CP.parseToplevel body

  return $ Generator { runGenerator = \wait -> \f -> 
    case rep of
      Forever -> forever (gen del wait f)
      Times r -> do { t <- fromRange r; times t (gen del wait f) }
  }
  where
    times :: (Monad m) => Int -> m a -> m a
    times 1 m = m
    times x m = m >> times (x - 1) m
