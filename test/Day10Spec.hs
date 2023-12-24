module Day10Spec where

import Day10
import Test.Hspec

spec :: Spec
spec = do
  describe "Day10" $ do
    it "should do sample 1" $ do
      let expected = 4
      day10 _input `shouldBe` expected
    it "should do sample 2" $ do
      let expected = 8
      day10 _input2 `shouldBe` expected
  describe "Day10b" $ do
    it "should do sample 1" $ do
      let expected = 10
      day10b _input5 `shouldBe` expected
