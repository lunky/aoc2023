module Day6Spec where

import Day6
import Test.Hspec

spec :: Spec
spec = do
  describe "Day6" $ do
    it "should do sample 1" $ do
      let expected = 288 
      day6 _input `shouldBe` expected
  describe "Day6b" $ do
    it "should do sample 1" $ do
      let expected = 71503
      day6b _input `shouldBe` expected
