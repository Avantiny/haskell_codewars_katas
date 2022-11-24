module Main where

import SixKyu ( findNb )
import FourKyu ( snail )
import FiveKyu
import Kata
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "katas" $ do
    it "findNb" $ do
      findNb 9 `shouldBe` 2
      findNb 36 `shouldBe` 3
    it "dominik" $ do
      dominik `shouldBe` 1
    it "Snail - first example" $ do
      let array = [[1,2,3],
                  [4,5,6],
                  [7,8,9]]
      let expected = [1,2,3,6,9,8,7,4,5]
      snail array `shouldBe` expected
    it "Snail - Second example" $ do
      let array = [[1,2,3],
                  [8,9,4],
                  [7,6,5]]
      let expected = [1,2,3,4,5,6,7,8,9]
      snail array `shouldBe` expected

    
      