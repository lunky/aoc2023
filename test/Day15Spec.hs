module Day15Spec where

import Day15
import Test.Hspec

spec :: Spec
spec = do
  describe "Day15" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day15 _input `shouldBe` expected
  describe "Day15b" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day15b _input `shouldBe` expected
