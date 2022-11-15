module SixKyu where
import Data.List

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