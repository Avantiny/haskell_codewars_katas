module FiveKyu where

import Control.Monad.State
import GHC.Float
import Debug.Trace
import Data.List

-- Product of consecutive Fib numbers

fib :: Integer -> Integer
fib n = fibs !! fromIntegral n
      where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

productFib :: Integer -> (Integer, Integer, Bool)
productFib n = productFib' n 1 1 
                  
productFib' :: Integer -> Integer -> Integer -> (Integer, Integer, Bool)
productFib' n c b | prod > n = (b,a,False)
                  | prod == n = (b,a,True)
                  | otherwise = productFib' n (c+1) a
                where a = fib (c + 1)
                      prod = a * b

-- Moving Zeros To The End

moveZeros :: [Int] -> [Int]
moveZeros x = filter (/= 0) x ++ filter (== 0) x

-- Number of trailing zeros of N!

zeros' :: Int -> Int -> Int
zeros' n m
  | n > 5^(m+1) = num + zeros' n (m + 1)
  | n < 5^m = 0
  | otherwise = num
  where
      num = n `div` 5^m
      
zeros :: Int -> Int
zeros n = zeros' n 1

-- Conway's Look and Say - Generalized

lookSay :: Integer -> Integer
lookSay n = read $ foldr1 (++) $ fmap (\c -> show (length c) ++ [head c]) (group (show n))

-- Human Readable Time

tenCondition :: Int -> String
tenCondition x | x == 0 = show x ++ "0"
               | x < 10 = "0" ++ show x
               | otherwise = show x
               
humanReadable :: Int -> String
humanReadable x = tenCondition totalHours ++ ":" ++ tenCondition totalMinutes ++ ":" ++ tenCondition seconds
                  where totalHours = x `div` 3600 
                        totalMinutes = x `div` 60 
                        minutes = totalMinutes - (60 * totalHours)
                        seconds = x - (3600 * totalHours + 60 * minutes)