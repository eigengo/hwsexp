module Custom.GeneratorSpec(spec) where

import Test.Hspec
import GeneratorSpecSupport
import Generator (newPermgen)
import qualified Data.Map as M

spec :: Spec
spec = do
  describe "Do expression" $ do
    it "Generates fixed count of ranges" $ do
      generate' "do { def foo(a b) a * b + 3; foo(1, 3); } once" `shouldReturn` [1]

  describe "Permgen generator" $ do
    it "Makes use of the permgen space" $ do
      let expr = "do { def foo(a b) a * b + 3; foo(1, 3); } once"
      (permgen1, values1) <- permgenGenerate' newPermgen expr
      (permgen2, values2) <- permgenGenerate' permgen1 expr

      (M.keys permgen1) `shouldBe` (M.keys permgen2)
      values1 `shouldBe` values2  
