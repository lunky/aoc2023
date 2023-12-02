module LibSpec where
import Lib
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Lib" $ do
    it "should demonstrate that Lib is working" $ do
      doubleFunc 1 `shouldBe` 2

