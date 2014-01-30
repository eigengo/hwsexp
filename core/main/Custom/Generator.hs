module Custom.Generator(mkGenerator) where

import Prelude hiding (lookup)
import Custom.Codegen
import Custom.Emit
import Custom.Syntax
import GeneratorSupport

mkGenerator :: [Expr] -> GeneratorCallback a -> IO a
mkGenerator expr f = do
  mod <- codegen (emptyModule "jit") expr
  val <- run mod
  f [fromIntegral val]
