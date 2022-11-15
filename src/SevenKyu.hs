module SevenKyu where

import Data.Char
import Data.List

-- Square Every Digit

squareDigits :: String -> String
squareDigits (x:xs) = show  (d^2) ++ squareDigits xs where
  d = read [x]
squareDigits [] = []

squareDigit :: Int -> Int
squareDigit x
  | x < 0 = -(squareDigit (negate x))
  | otherwise = read(squareDigits(show x))

-- Get the Middle Character

getMiddle :: String -> String
getMiddle xs = take (signum ((l + 1) `mod` 2) + 1) $ drop ((l - 1) `div ` 2) xs
  where l = length xs

-- esreveR

reverse' :: [a] -> [a]
reverse' = go []
    where
        go acc [] = acc
        go acc (x:xs) = go (x:acc) xs

-- You're a square!

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

isSquare :: Integral n => n -> Bool
isSquare n = isInt (sqrt $ fromIntegral n)

-- Vowel one

isVowel''' x = x `elem` "aeiouAEIOU"

vowelOne :: String -> String
vowelOne = fmap ( \c -> if isVowel''' c then '1' else '0')

-- Disemvowel Trolls

isVowel' :: Char -> Bool
isVowel' c = c `elem` "aeiouAEIOU"

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot pred = filter $ not . pred

disemvowel :: String -> String
disemvowel = filterNot isVowel'

-- shorter concat [reverse longer]

reverseLonger :: String -> String -> String
reverseLonger a b = if length b > length a then a ++ reverse b ++ a
                      else b ++ reverse a ++ b

-- Vowel Count

isVowel'' :: Char -> Bool
isVowel'' c = c `elem` "aeiou"

getCount :: String -> Int
getCount c = length $ filter isVowel'' c

-- 
