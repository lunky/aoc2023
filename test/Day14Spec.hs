module Day14Spec where

import Day14
import Test.Hspec

spec :: Spec
spec = do
  describe "Day14" $ do
    it "should do sample 1" $ do
      let expected = 136
      day14 _input `shouldBe` expected
  describe "Day14b" $ do
    it "should do sample 1" $ do
      let expected = 64
      day14b _input `shouldBe` expected
