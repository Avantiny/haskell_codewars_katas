module Main where

import Kata2
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