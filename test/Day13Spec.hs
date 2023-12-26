module Day13Spec where

import Day13
import Test.Hspec

spec :: Spec
spec = do
  describe "Day13" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day13 _input `shouldBe` expected
  describe "Day13b" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day13b _input `shouldBe` expected
