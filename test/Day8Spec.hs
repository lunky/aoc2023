module Day8Spec where

import Day8
import Test.Hspec

spec :: Spec
spec = do
  describe "Day8" $ do
    it "should do sample 1" $ do
      let expected = 2
      day8 _input `shouldBe` expected
    it "should do sample 2" $ do
      let expected = 6
      day8 _input2 `shouldBe` expected
  describe "Day8b" $ do
    it "should do sample 1" $ do
      let expected = 6
      day8b _input3 `shouldBe` expected
