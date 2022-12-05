module SixKyu where
import Data.List
import Numeric
import Data.Char

-- Build a pile of Cubes

rec :: Integer -> Integer -> Integer -> Integer
rec n lim c  | (c + cubed) > lim = -1
             | (c + cubed) == lim = n
             | c < lim = rec (n + 1) lim (cubed + c)
             where cubed = n^3

findNb :: Integer -> Integer
findNb x = rec 1 x 0

-- Create Phone Number

createPhoneNumber :: [Int] -> String
createPhoneNumber xs = "(" ++ intercalate "" (take 3 (fmap show xs)) ++ ") " ++ intercalate "" ( fmap show [xs !! 3, xs !! 4, xs !! 5]) ++ "-" ++ intercalate "" ( fmap show [xs !! 6, xs !! 7, xs !! 8, xs !! 9]) 

-- Array.diff

difference :: Eq a => [a] -> [a] -> [a]
difference a b = [x | x <- a, x `notElem` b]

-- Sum of Digits / Digital Root

count' :: String -> Integer
count' [] = 0
count' xs = foldr (\ x -> (+) (read [x])) 0 xs

digitalRoot :: Integer -> Integer
digitalRoot a | a <= 9 = c
              | otherwise = digitalRoot c
                    where c = count' $ show a

-- Multiples of 3 or 5

solution :: Integer -> Integer
solution number = sum y
          where y = [x | x <- [1..(number-1)], x `mod` 3 == 0 || x `mod` 5 == 0]

-- Find The Parity Outlier

findOutlier :: [Int] -> Int 
findOutlier x = if  length l > 1 then head (filter even x)
                else  head l
                where l = filter odd x

-- Pyramid Array

pyramid :: Int -> [[Int]]
pyramid n | n == 0 = []
          | otherwise = pyramid (n-1) ++ [replicate n 1]

-- Find the odd int

findOdd :: [Int] -> Int
findOdd xs = head $ concat $ filter (odd . length) (group $ sort xs)

-- Stop gninnipS My sdroW!

spinWords :: String -> String
spinWords str = if last s == ' ' then init s else s
                  where s = concatMap (\c -> if length c >= 5 then reverse c ++ " "else c ++ " ") (words str)

-- Bit Counting

countBits :: Int -> Int
countBits x = length $ filter (== '1')  (showIntAtBase 2 intToDigit x "")

-- CamelCase Method

capitalized :: String -> String
capitalized (head:tail) = toUpper head : map toLower tail
capitalized [] = []

camelCase :: String -> String
camelCase x = concatMap capitalized (words x)

-- Find the unique number

getUnique :: [Int] -> Int
getUnique (x : xs)
  | x /= head xs = if x == head (tail xs) then head xs else x
  | x /= head (tail xs) = if x == head xs then head (tail xs) else x
  | otherwise = getUnique xs

-- Character with longest consecutive repetition

maxi :: Int -> [String] -> String -> String
maxi n [] c = c
maxi n (x:xs) c = if length x > n then maxi (length x) xs x else maxi n xs c


longestRepetition :: String -> Maybe (Char, Int)
longestRepetition "" = Nothing
longestRepetition input = Just (head x, length x)
                        where x = maxi 0 (group input) ""

-- Begin your day with a challenge, but an easy one.

-- replicateByLength :: Int -> String -> String
-- replicateByLength n [] = []
-- replicateByLength n (x:xs) = replicate n x ++ replicateByLength (n - 1) xs

-- reverseMulti :: Integer -> Integer
-- reverseMulti x = read $ replicateByLength (length a) a
--                  where a = reverse (show x)

byNine :: Integer -> Integer
byNine n | n < 10 = n
         | otherwise = read $ concatMap show (sub : [modulo | modulo /= 0])
           where sub = read $ concatMap show (replicate (fromIntegral n `div` 9) 9)
                 modulo = n `mod` 9

oneTwoThree :: Integer -> [Integer]
oneTwoThree 0 = [0,0]
oneTwoThree n = byNine n
  : [read $ concatMap show (replicate (fromIntegral n) 1)]
