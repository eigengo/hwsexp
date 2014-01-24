module GeneratorSpec(spec) where

import Generator
import Control.Applicative ((<$>))
import Test.Hspec

spec :: Spec
spec = do
  describe "Do expression" $ do
    it "Generates fixed count of ranges" $ do
      generate' "do { def foo(a b) a * b; foo(1, 1); } once" `shouldReturn` [1]

  describe "Simple expression" $ do
    it "Generates fixed count of fixed ranges" $ do
      generate' "{ evendistr 1 1 } once" `shouldReturn` [1]
      generate' "{ evendistr 1 1 } 1 times" `shouldReturn` [1] -- stupid to say 1 times, but hey ho.
      generate' "{ evendistr [1..1] [1..1] } once" `shouldReturn` [1]
      generate' "{ evendistr 2 1 } once" `shouldReturn` [1, 1]

  describe "Even random expression" $ do
    it "Generates even distribution of values" $ do
      avg <$> generate' "{ evendistr 8000 [0..100] } once" >>= (`shouldSatisfy` (< 3) . abs . (50 -))

  describe "Expression parsing" $ do
    it "Reports errors" $ do
      error' "evendis" `shouldBe` "\"<stdin>\" (line 1, column 1):\nunexpected \"e\"\nexpecting \"do\" or \"{\""
      --error' "evendistr 100" `shouldBe` "Failed reading: empty"
      --error' "evendistr 100 foo" `shouldBe` "Failed reading: empty"
      --error' "evendistr 100 100 theff" `shouldBe` "endOfInput"
      --error' "evendistr 100 100 forever every x" `shouldBe` "endOfInput"
      --error' "evendistr 100 100 forever every [100..]" `shouldBe` "endOfInput"

  where
    error' :: String -> String
    error' expr =
      let Left err = generator expr
      in  show err

    generate' :: String -> IO [Int]
    generate' expr = 
      case runGenerator <$> generator expr of
        Right f -> f (const $ return ()) return
        Left  e -> fail $ show e
      
      --let Right f = runGenerator <$> generator expr 
      --in  f (const $ return ()) return
      
    avg xs = sum xs `div` length xs