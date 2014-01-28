{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Generator(generator, Generator(..), GeneratorDelay) where

import Syntax
import GeneratorSupport
import Text.Parsec.Error

import Control.Monad.State
import Control.Applicative ((<$>))

import qualified Parser as P
import qualified Standard.Parser as SP
import qualified Custom.Parser as CP
import qualified Standard.Generator as SG
import qualified Custom.Generator as CG

import qualified Data.Map as M

-- |Permgen space holding the prepared generators given the generating expression
type Permgen a b = M.Map Distribution (Generator a b)

-- |Generator is a type that performs a step of the generation process
newtype Generator a b = Generator { 

  -- |Performs the generator step
  runGenerator :: GeneratorDelay     -- ^The delaying function. You can pass in @threadDelay@, for example.
               -> (a -> IO b)        -- ^The operation to execute in every step
               -> IO b               -- ^The final result
}

-- |Perpares new permgen space for the caching generator
newPermgen :: State Distribution (Permgen a b)
newPermgen = return M.empty

-- |Prepares a generator by parsing the input string. It returns a value that
--  you can use to get the generated values.
--   
--  Typically, you'd have code similar to
--  @
--      case (generator "{evendistr 5 [1..20]} every 50ms") of
--          Left err  -> ...                                         -- what do you want, a cookie?
--          Right gen -> runGenerator gen threadDelay (\nums -> ...) -- good user, have numbers
-- @
generator :: String                             -- ^The expression to parse
          -> Either ParseError (Generator [Int] b)  -- ^The result with errors or the ready Generator
generator input = do
  (Expression exp rep del) <- P.parseExpression input
  gen <- case exp of
           Standard body -> SG.mkGenerator <$> SP.parseToplevel body 
           Custom body   -> CG.mkGenerator <$> CP.parseToplevel body

  return $ Generator { runGenerator = \sleep -> \f -> 
    case rep of
      Forever -> forever (delayed del sleep (gen f))
      Times r -> do { t <- fromRange r; times t (delayed del sleep (gen f)) }
  }


times :: (Monad m) => Int -> m a -> m a
times 1 m = m
times x m = m >> times (x - 1) m

delayed :: Delay -> GeneratorDelay -> IO a -> IO a
delayed (Fixed delayRange) sleep !m = do
  delay <- fromRange delayRange
  sleep delay
  m