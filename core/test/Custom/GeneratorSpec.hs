module Custom.GeneratorSpec(spec) where

import Test.Hspec
import GeneratorSpecSupport

spec :: Spec
spec = do
  describe "Pure do expression" $ do
    it "Generates fixed count of ranges" $ do
      generate' "do { def foo(a b) a * b + 3; foo(1, 2); } once" `shouldReturn` [5]

  -- TODO: Fails now, need to implement __tick.
  describe "Impure do expression" $ do
    it "Generates fixed count of random values" $ do
      values <- generate' "do { __tick; } 100 times"
      monotonouslyIncreasing (head values) values `shouldBe` True
    where
      monotonouslyIncreasing x (h:t) 
        | h >= x = monotonouslyIncreasing h t
        | h <  x = False
      monotonouslyIncreasing _ _ = True