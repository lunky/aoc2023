module Day7Spec where

import Day7
import Test.Hspec

spec :: Spec
spec = do
  describe "Day7" $ do
    it "should do sample 1" $ do
      let expected = 6440
      day7 _input `shouldBe` expected

  describe "Day7b" $ do
    it "should do sample 1" $ do
      let expected = 5905
      day7b _input `shouldBe` expected
    it "should do sample 2" $ do
      let expected = 249631254
      content <- readFile "data/day7.txt"
      day7b content `shouldBe` expected
