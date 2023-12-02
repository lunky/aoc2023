module Day1Spec where
import Day1 
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "test harness" $ do
    it "should demonstrate that test are working" $ do
      1 `shouldBe` 1
  describe "day1" $ do
    it "should work for pattern 1" $ do
      let input = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
      let expected = 142 
      day1 input `shouldBe` expected
  describe "day1b" $ do
    it "should work for pattern 1" $ do
      let expected = 281
      let input = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"
      day1b input `shouldBe` expected

