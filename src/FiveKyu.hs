module FiveKyu where

import Control.Monad.State

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