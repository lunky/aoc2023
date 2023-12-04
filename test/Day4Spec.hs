module Day4Spec where

import Day4
import Test.Hspec

spec :: Spec
spec = do
  describe "Day4" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day4 _input `shouldBe` expected
  describe "Day4b" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day4b _input `shouldBe` expected
