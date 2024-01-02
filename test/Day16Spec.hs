module Day16Spec where

import Day16
import Test.Hspec

spec :: Spec
spec = do
  describe "Day16" $ do
    it "should do sample 1" $ do
      let expected = 46
      day16 _input `shouldBe` expected
  describe "Day16b" $ do
    it "should do sample 1" $ do
      let expected = 51
      day16b _input `shouldBe` expected
