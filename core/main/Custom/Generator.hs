module Custom.Generator(mkGenerator) where

import Custom.Syntax
import Syntax
import GeneratorSupport

mkGenerator :: [Expr] -> Delay -> GeneratorDelay -> ([Int] -> IO b) -> IO b
mkGenerator _ _ _ _ = fail "Bantha poodoo!"