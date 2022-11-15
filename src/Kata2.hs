module Kata2 where
import Data.Maybe
import Data.Char ( digitToInt, chr, toUpper )
import Control.Applicative
import Data.List
import Debug.Trace

-- avg :: [Float] -> Float
-- avg a = (/) (sum a) $ (fromIntegral $ length a )

oddCount :: Int -> Int 
oddCount n = (n + 1) `div` 2

-- isVowel x = elem x "aeiouAEIOU"

-- vowelOne :: String -> String
-- vowelOne a = fmap ( \c -> if (isVowel c) then '1' else '0') a

countPositivesSumNegatives :: Maybe [Int] -> [Int]
countPositivesSumNegatives (Just []) = []
countPositivesSumNegatives (Just xs) = [length . filter (>0), sum . filter (<0)]<*>[xs]
countPositivesSumNegatives Nothing = []

-- countPositivesSumNegatives :: Maybe [Int] -> [Int]
-- countPositivesSumNegatives xs' = [length $ filter (>0) xs, sum $ filter (<0) xs]
--                            where xs = fromMaybe [0,0] xs'


isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

isSquare :: Integral n => n -> Bool
isSquare n = isInt (sqrt $ fromIntegral n)


-- reorder :: [String] -> [String]
-- reorder partz 
--         | length partz > 0 = (take (length partz) partz) ++ tail (init partz) ++ (take 1 partz)
--         | otherwise = []


-- reorder :: [String] -> [String]
-- reorder partz 
--         | length partz > 0 = ([head partz]) ++ tail (init partz) ++ [last partz]
--         | otherwise = []


-- highAndLow :: String -> String
-- highAndLow input = fmap chr $ maximum (fmap digitToInt input)

-- isPalindrom :: String -> Bool
-- isPalindrom str = reverse str == str

-- digs :: Integral x => x -> [x]
-- digs 0 = []
-- digs x = digs (x `div` 10) ++ [x `mod` 10]

-- digits :: Integer -> [Int]
-- digits = map (read . (:[])) . show

-- fromDigits :: [Int] -> Int
-- fromDigits = foldl' addDigit 0
--    where addDigit num d = (10 ^ (floor $ log (fromIntegral d) / log 10 + 1))*num + d

-- squareDigit :: Int -> Int
-- squareDigit n = fromDigits $ fmap (^2) $ digits $ fromIntegral n


-- squareDigit :: Int -> Int
-- squareDigit n = fromDigits $ fmap (^2) $ digs n

-- joiner :: [Int] -> Int
-- joiner = read . concatMap show

-- squareDigit :: Int -> Int
-- squareDigit n = joiner $ (fmap (^2) $ digits $ fromIntegral n)

-- reverse :: [a] -> [a]
-- reverseList = foldl (\acc x -> x : acc) []
-- reverse = foldl (flip (:)) []

-- apple :: Either String Int -> String
-- apple (Right a) = if a^2 > 1000 then "It's hotter than the sun!!"
--                   else  "Help yourself to a honeycomb Yorkie for the glovebox."
-- apple (Left a) = if read a ^2 > 1000 then "It's hotter than the sun!!"
--                   else  "Help yourself to a honeycomb Yorkie for the glovebox."


digits :: Integer -> [Int]
digits = map (read . (:[])) . show

fromDigits :: [Int] -> Int
fromDigits = foldl' addDigit 0
   where addDigit num d = 10 ^ floor (logBase 10 (fromIntegral d) / log 10 + 1)*num + d
   
-- joiner :: [Int] -> Int
-- joiner = read . concatMap show

squareDigit :: Int -> Int
squareDigit n = fromDigits $ fmap (^2) $ digits $ fromIntegral n

greet :: IO()
greet = do
  putStrLn "hello world!"

-- getMiddle :: String -> String
-- getMiddle s = if odd $ length s then s !! (length s/2) ++ s !! (length s $ (/2) + 1)
--                 else s !! length s

-- createPhoneNumber :: [Int] -> String
-- createPhoneNumber xs = "(" ++ take 3 (fmap show xs) ++ ")"

count' :: String -> Int
count' (x:xs) = read [x] + count' xs

digitalRoot :: Int -> Int
digitalRoot a = count' $ show a

getAge :: (Integral a, Read a) => String -> a
getAge (x:xs) = read [x]

rec :: Integer -> Integer -> Integer -> Integer
rec n lim c  | (c + cubed) > lim = -1
             | (c + cubed) == lim = n
             | c < lim = rec (n + 1) lim (cubed + c)
             where cubed = n^3

findNb :: Integer -> Integer
findNb x  | x <= 0 = -1
          | otherwise = rec 1 x 0


solution :: Integer -> Integer
solution number = trace (show y) $ sum y
          where y = [x | x <- [1..number], x `mod` 3 == 0 || x `mod` 5 == 0]


-- findOutlier :: [Int] -> Int 
-- findOutlier (x:y:xs) = if odd x && odd y then head (filter odd xs)
--                     else head (filter even xs)

findOutlier :: [Int] -> Int 
findOutlier (x:y:xs) = if odd x && odd y then head (filter odd (x:y:xs))
                    else head (filter even (x:y:xs))

getArrayBody :: [Int] -> [Int]
getArrayBody [] = []
getArrayBody [_] = []
getArrayBody [_,_] = []
getArrayBody x = init (tail x)

snail :: [[Int]] -> [Int]
snail [] = []
snail [x] = x
snail x = head x ++ fmap last x ++ reverse (init $ last x) ++ reverse (fmap head snailBody) ++ snail (fmap getArrayBody snailBody)
              where snailBody = init $ tail x

generateNonSpiral :: Int -> [Int] -> [[Int]]
generateNonSpiral n (x:xs) = [x..(n+x)]:generateNonSpiral n xs

-- highAndLow :: String -> String
-- highAndLow :: String -> String
-- highAndLow input = show (maximum nums) ++ " " ++ show (minimum nums)
--                 where nums = read <$> words input

-- accum :: [Char] -> [Char]
-- accum (x:xs) = toUpper x ++ replicate (elemIndex x (x:xs)) (x:xs) ++ "-" ++ accum xs

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

productFib :: Integer -> (Integer, Integer, Bool)
productFib n = productFib' n 1
                  
productFib' :: Integer -> Integer -> (Integer, Integer, Bool)
productFib' n c | a * b > n = (a,b,False)
               | a * b == 0 = (a,b,True)
               | otherwise = productFib' n (c+1)
                where a = fib (n + 1)
                      b = fib n