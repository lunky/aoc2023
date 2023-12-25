module Day11Spec where

import Day11
import Test.Hspec

spec :: Spec
spec = do
  describe "Day11" $ do
    it "should do sample 1" $ do
      let expected = 374
      day11 _input `shouldBe` expected
  describe "Day11b" $ do
    it "should do sample 1" $ do
      let expected = 8410
      let multiplier = 100
      day11b _input multiplier `shouldBe` expected
