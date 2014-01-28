 {-# LANGUAGE BangPatterns #-}
module Standard.Generator(mkGenerator) where

import Control.Monad (replicateM)
import Standard.Syntax
import GeneratorSupport

mkGenerator :: Expr -> ([Int] -> IO b) -> IO b
mkGenerator (EvenDistr countRange valueRange) !f = do
  count  <- fromRange countRange
  values <- replicateM count (fromRange valueRange)
  f values
