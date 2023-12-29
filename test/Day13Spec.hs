module Day13Spec where

import Day13
import Test.Hspec

spec :: Spec
spec = do
  describe "Day13" $ do
    it "should do sample 1" $ do
      let expected = 405
      day13 _input `shouldBe` expected
  describe "Day13b" $ do
    it "should do sample 1" $ do
      let expected = 400
      day13b _input `shouldBe` expected
