module Day12Spec where

import Day12
import Test.Hspec

spec :: Spec
spec = do
  describe "Day12" $ do
    it "should do sample 1" $ do
      let expected = 21
      day12 _input `shouldBe` expected
  describe "Day12b" $ do
    it "should unfold sample" $ do
      let input = [(".#",[1])]
      let expected = [(".#?.#?.#?.#?.#", [1,1,1,1,1])]
      unfold input `shouldBe` expected
    it "should do sample 1" $ do
      let expected = 525152 
      day12b _input `shouldBe` expected
