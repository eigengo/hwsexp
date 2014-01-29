module Custom.Generator(mkGenerator) where

import Prelude hiding (lookup)
import Custom.Codegen
import Custom.Emit
import Custom.Syntax
import GeneratorSupport

mkGenerator :: [Expr] -> GeneratorFunction Int b
mkGenerator expr f = do
  _ <- codegen (emptyModule "jit") expr
  f []
