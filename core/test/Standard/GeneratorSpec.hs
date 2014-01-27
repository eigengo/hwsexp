module Standard.GeneratorSpec(spec) where

import Test.Hspec
import GeneratorSpecSupport
import Control.Applicative ((<$>))

spec :: Spec
spec = do
  describe "Simple expression" $ do
    it "Generates fixed count of fixed ranges" $ do
      generate' "{ evendistr 1 1 } once" `shouldReturn` [1]
      generate' "{ evendistr 1 1 } 1 times" `shouldReturn` [1] -- stupid to say 1 times, but hey ho.
      generate' "{ evendistr 1 1 } [1..1] times" `shouldReturn` [1] -- stupid to say 1 times, but hey ho.
      generate' "{ evendistr [1..1] [1..1] } once" `shouldReturn` [1]
      generate' "{ evendistr 2 1 } once" `shouldReturn` [1, 1]
      generate' "{ evendistr 2 1 } once every 1000ms" `shouldReturn` [1, 1]
      generate' "{ evendistr 2 1 } once every [0..1000]ms" `shouldReturn` [1, 1]

  describe "Even random expression" $ do
    it "Generates even distribution of values" $ do
      avg <$> generate' "{ evendistr 8000 [0..100] } once" >>= (`shouldSatisfy` (< 3) . abs . (50 -))

  describe "Expression parsing" $ do
    it "Reports errors" $ do
      error' "evendis" `shouldBe` "\"<stdin>\" (line 1, column 1):\nunexpected \"e\"\nexpecting \"do\" or \"{\""
      error' "{evendistr 100}" `shouldBe` "\"<stdin>\" (line 1, column 14):\nunexpected end of input\nexpecting digit or range (n; [m..n] | n > m)"
      error' "{evendistr 100 foo}" `shouldBe` "\"<stdin>\" (line 1, column 15):\nunexpected \"f\"\nexpecting range (n; [m..n] | n > m)"
      error' "{evendistr 100 100} theff" `shouldBe` "\"<stdin>\" (line 1, column 21):\nunexpected 't'\nexpecting repetition, delay or end of input"
      error' "{evendistr 100 100} forever every x" `shouldBe` "\"<stdin>\" (line 1, column 35):\nunexpected \"x\"\nexpecting range (n; [m..n] | n > m)"
      error' "{evendistr 100 100} forever every [100..]" `shouldBe` "\"<stdin>\" (line 1, column 41):\nunexpected \"]\"\nexpecting integer"
