module ParserSpec(spec) where

import Text.Parsec
import ParserSupport
import Parser
import Syntax
import Test.Hspec

spec :: Spec
spec = do
  describe "The common parser" $ do
    it "Should pick standard expressions" $ do
      let Right (Expression (Standard body) _ _) = parseExpression "{ evendistr 1 1 }"
      body `shouldBe` " evendistr 1 1 "

    it "Should pick do expressions" $ do
      let Right (Expression (Custom body) _ _) = parseExpression "do { def x(a, b) a * b; x(1, 1); }"
      body `shouldBe` " def x(a, b) a * b; x(1, 1); "

    it "Should parse repetitions" $ do
      let Right (Expression _ once _) = parseExpression "do { } once"
      once `shouldBe` (Times $ Exact 1)

      let Right (Expression _ twice _) = parseExpression "do { } 2 times"
      twice `shouldBe` (Times $ Exact 2)

      let Right (Expression _ oneToThree _) = parseExpression "do { } [1..3] times"
      oneToThree `shouldBe` (Times $ Between 1 3)

      let Right (Expression _ forever _) = parseExpression "do { } forever"
      forever `shouldBe` Forever

    it "Should parse delays" $ do
      let Right (Expression _ _ (Fixed exact)) = parseExpression "do { } every 1000ms"
      exact `shouldBe` (Exact 1000000)

      let Right (Expression _ _ (Fixed oneToThree)) = parseExpression "do { } every [1..3]ms"
      oneToThree `shouldBe` (Between 1000 3000)


  describe "Range parser" $ do
    it "Parses exact" $ do
      let Right one = parse range "<stdin>" "1"
      one `shouldBe` Exact 1

    it "Parses ranges" $ do
      let Right oneToThree = parse range "<stdin>" "[1..3]"
      oneToThree `shouldBe` Between 1 3