 {-# LANGUAGE BangPatterns #-}
module Standard.Generator(mkGenerator) where

import Control.Monad (replicateM)
import Syntax
import Standard.Syntax
import GeneratorSupport

mkGenerator :: Expr -> Delay -> GeneratorDelay -> ([Int] -> IO b) -> IO b
mkGenerator (EvenDistr countRange valueRange) (Fixed delayRange) wait !f = do
  count  <- fromRange countRange
  delay  <- fromRange delayRange
  values <- replicateM count (fromRange valueRange)
  wait delay
  f values
