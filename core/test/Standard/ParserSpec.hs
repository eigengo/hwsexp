module Standard.ParserSpec(spec) where

import Standard.Parser
import Standard.Syntax
import Syntax
import Test.Hspec

spec :: Spec
spec = do
  describe "Standard parser" $ do
    it "Parses exact even distributions" $ do
      let Right (EvenDistr count vals) = parseToplevel "evendistr 1 1"
      count `shouldBe` Exact 1
      vals `shouldBe` Exact 1

    it "Parses ranged even distributions" $ do
      let Right (EvenDistr count vals) = parseToplevel "evendistr [1..10] [1..20]"
      count `shouldBe` Between 1 10
      vals `shouldBe` Between 1 20