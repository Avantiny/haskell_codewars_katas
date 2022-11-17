{-# LANGUAGE RankNTypes #-}
module FourKyu where

-- Snail

getArrayBody :: [Int] -> [Int]
getArrayBody x = init (tail x)

snail :: [[Int]] -> [Int]
snail [] = []
snail [x] = x
snail x = head x ++ fmap last (tail x) ++ tail (reverse (last x)) ++ reverse (fmap head snailBody) ++ snail (fmap getArrayBody snailBody)
              where snailBody = init $ tail x

-- Church numbers

newtype Number = Nr (forall a. (a -> a) -> a -> a)

zero :: Number
zero = Nr (\ _ z -> z)

succ' :: Number -> Number
succ' (Nr a) = Nr (\ f x -> f (a f x))

one :: Number
one = succ' zero

add :: Number -> Number -> Number
add (Nr a) (Nr b) = Nr (\ f x -> b f (a f x))

mult :: Number -> Number -> Number
mult (Nr a) (Nr b) = Nr (b . a)

pow :: Number -> Number -> Number
pow (Nr a) (Nr b) = Nr (b a)

-- Multiplying numbers as strings

multiply :: String -> String -> String
multiply xs ys = show $ read xs * read ys