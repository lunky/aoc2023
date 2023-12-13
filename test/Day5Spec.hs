module Day5Spec where

import Day5
import Test.Hspec
import System.Timeout

spec :: Spec
spec = do
  describe "Day5" $ do
    it "should do sample 1" $ do
      let expected = 35
      day5 _input `shouldBe` expected
    it "should resolve seeds" $ do
      let input = 14
      let expected = 43
      resolveLocation input (loadMap _input) `shouldBe` expected
    it "should resolve seeds" $ do
      let input = 43
      let expected = 14
      resolveSeed input (loadMap _input) `shouldBe` expected
    it "should resolve location" $ do
      let input = 79
      let expected = 82
      resolveLocation input (loadMap _input) `shouldBe` expected
    it "should resolve location" $ do
      let input = 13
      let expected = 35
      resolveLocation input (loadMap _input) `shouldBe` expected
  describe "Day5b" $ do
    it "should do sample 1" $ do
      let expected = 46 
      day5b _input `shouldBe` expected
