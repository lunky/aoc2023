module Day16Spec where

import Day16
import Test.Hspec

spec :: Spec
spec = do
  describe "Day16" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day16 _input `shouldBe` expected
  describe "Day16b" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day16b _input `shouldBe` expected
