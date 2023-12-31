module Day15Spec where

import Day15
import Test.Hspec

spec :: Spec
spec = do
  describe "Day15" $ do
    it "should do sample 1" $ do
      let expected = 52
      hash _input `shouldBe` expected
    it "should do sample 1" $ do
      let expected = 1320
      day15 _input2 `shouldBe` expected
  describe "Day15b" $ do
    it "should do sample 1" $ do
      let expected = 145
      day15b _input2 `shouldBe` expected
