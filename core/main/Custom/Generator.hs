module Custom.Generator(mkGenerator) where

import Prelude hiding (lookup)
import Custom.Codegen
import Custom.Emit
import Custom.Syntax

mkGenerator :: [Expr] -> ([Int] -> IO b) -> IO b
mkGenerator expr f = do
  _ <- codegen (emptyModule "jit") expr
  f []
