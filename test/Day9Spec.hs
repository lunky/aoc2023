module Day9Spec where

import Day9
import Test.Hspec

spec :: Spec
spec = do
  describe "Day9" $ do
    it "should do sample 1" $ do
      let expected = 114
      day9 _input `shouldBe` expected
  describe "Day9b" $ do
    it "should do sample 1" $ do
      let expected = 2
      day9b _input `shouldBe` expected
