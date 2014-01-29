module GeneratorSpecSupport(generate', permgenGenerate', error', avg) where

import Generator
import Control.Applicative ((<$>))

error' :: String -> String
error' expr =
  let Left err = generator expr
  in  show err

generate' :: String -> IO [GeneratorValue]
generate' expr = 
  let Right f = runGenerator <$> generator expr
  in  f (const $ return ()) return

permgenGenerate' :: Permgen GeneratorValue [GeneratorValue] -> String -> IO (Permgen GeneratorValue [GeneratorValue], [GeneratorValue])
permgenGenerate' permgen expr = 
  let Right (permgen', gen) = permgenGenerator permgen expr
  in  (runGenerator gen (const $ return ()) return) >>= (\nums -> return (permgen', nums))

avg :: [Int] -> Int
avg xs = sum xs `div` length xs