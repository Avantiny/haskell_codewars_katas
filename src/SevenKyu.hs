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

-- Fizz Buzz

fizzbuzz' :: Int -> String
fizzbuzz' x | x `mod` 5 == 0 && x `mod` 3 == 0 = "FizzBuzz"
            | x `mod` 3 == 0 = "Fizz"
            | x `mod` 5 == 0 = "Buzz"
            | otherwise = show x


fizzbuzz :: Int -> [String]
fizzbuzz n = fmap fizzbuzz' [1..n]

-- Highest and Lowest

highAndLow :: String -> String
highAndLow input = show (maximum nums) ++ " " ++ show (minimum nums)
                where nums = fmap read (words input) :: [Int]

-- Descending Order

getSorted :: Integer -> [Int]
getSorted x = reverse . sort $ fmap digitToInt (show x)

descendingOrder :: Integer -> Integer
descendingOrder x = read $ fmap intToDigit (getSorted x)

-- Sort Numbers

sortNumbers :: [Int] -> Maybe [Int]
sortNumbers xs | not (null xs)  = Just (sort xs)
               | otherwise = Nothing

-- The fusc function part 1

fusc :: Int -> Int
fusc 0 = 0
fusc 1 = 1
fusc x | even x = fusc (x `div` 2)
       | otherwise = fusc y + fusc (y + 1)
                   where y = (x - 1) `div` 2

-- Isograms

isIsogram :: String -> Bool
isIsogram x = nub y == y
              where y = fmap toLower x

-- Next Palindromic Number.

nextPal :: Int -> Int
nextPal n = if numStr == reverse numStr then n+1 else nextPal (n+1)
            where numStr = show (n + 1)

-- Jaden Casing Strings

toJadenCase :: String -> String
toJadenCase js = unwords $ fmap (\(c:s) -> toUpper c:s) (words js)