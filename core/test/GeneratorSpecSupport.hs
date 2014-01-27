module GeneratorSpecSupport(generate', error', avg) where

import Generator
import Control.Applicative ((<$>))

error' :: String -> String
error' expr =
  let Left err = generator expr
  in  show err

generate' :: String -> IO [Int]
generate' expr = 
  let Right f = runGenerator <$> generator expr
  in  f (const $ return ()) return

avg :: [Int] -> Int
avg xs = sum xs `div` length xs