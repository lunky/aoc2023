module Day7Spec where

import Day7
import Test.Hspec

spec :: Spec
spec = do
  describe "Day7" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day7 _input `shouldBe` expected
  describe "Day7b" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day7b _input `shouldBe` expected
