module Day5Spec where

import Day5
import Test.Hspec

spec :: Spec
spec = do
  describe "Day5" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day5 _input `shouldBe` expected
  describe "Day5b" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day5b _input `shouldBe` expected
