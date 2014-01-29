module Custom.GeneratorSpec(spec) where

import Test.Hspec
import GeneratorSpecSupport
import Generator (newPermgen)
import qualified Data.Map as M

spec :: Spec
spec = do
  describe "Do expression" $ do
    it "Generates fixed count of ranges" $ do
      generate' "do { def foo(a b) a * b; foo(1, 1); } once" `shouldReturn` [1]

  describe "Permgen generator" $ do
    it "Makes use of the permgen space" $ do
      let expr = "do { def foo(a b) a * b; foo(1, 1); } once"
      (permgen1, values1) <- permgenGenerate' newPermgen expr
      (_, values2) <- permgenGenerate' permgen1 expr

      print (M.keys permgen1)
      values1 `shouldBe` values2