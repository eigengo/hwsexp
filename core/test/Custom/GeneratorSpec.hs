module Custom.GeneratorSpec(spec) where

import Test.Hspec
import GeneratorSpecSupport

spec :: Spec
spec = do
  describe "Do expression" $ do
    it "Generates fixed count of ranges" $ do
      generate' "do { def foo(a b) a * b; foo(1, 1); } once" `shouldReturn` [1]
