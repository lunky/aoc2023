module Day17Spec where

import Day17
import Test.Hspec

spec :: Spec
spec = do
  describe "Day17" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day17 _input `shouldBe` expected
  describe "Day17b" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day17b _input `shouldBe` expected
