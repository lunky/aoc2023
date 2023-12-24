module Day11Spec where

import Day11
import Test.Hspec

spec :: Spec
spec = do
  describe "Day11" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day11 _input `shouldBe` expected
  describe "Day11b" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day11b _input `shouldBe` expected
