module EightKyu where
import Data.Maybe
import Data.Char ( digitToInt, chr, toUpper )
import Control.Applicative
import Data.List
import Debug.Trace

-- Multiply

multiply :: Int -> Int -> Int
multiply a b = a * b

-- Basic Mathematical Operations

basicOp :: Char -> Int -> Int -> Int
basicOp '+' a b = a + b 
basicOp '-' a b = a - b
basicOp '*' a b = a * b 
basicOp '/' a b = div a b 

-- Convert a String to a Number!

stringToNumber :: String -> Integer
stringToNumber s = read s :: Integer

-- Beginner Series #2 Clock

past :: Int -> Int -> Int -> Int
past h m s = 1000 * (h * 3600 + m * 60 + s)

-- Localize The Barycenter of a Triangle

round' :: Double -> Integer -> Double
round' num sg = (fromIntegral . round $ num * f) / f
    where f = 10^sg

barTriang :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double)
barTriang (a, b) (c, d) (e, f) = (round'((a+c+e)/3) 4,round'((b+d+f)/3) 4)

-- Coefficients of the Quadratic Equation

quadratic :: Int -> Int -> (Int,Int,Int)
quadratic a b = (1, -(a+b), a*b)

-- String repeat

repeatStr :: Int -> String -> String
repeatStr n str = concat $ replicate n str

-- You Can't Code Under Pressure #1

doubleInteger :: Num a => a -> a
doubleInteger = (*) 2

-- You only need one - Beginner

check :: Eq a => [a] -> a -> Bool
check arr x = x `elem` arr

-- Return Negative

makeNegative :: (Num a, Ord a) => a -> a
makeNegative x = if x > 0 then -x
                    else x

-- Even or Odd

evenOrOdd :: Integral a => a -> [Char]
evenOrOdd a = if even a then "Even"
              else "Odd"

-- Calculate average

avg :: [Float] -> Float
avg a =  sum a / fromIntegral (length a)

-- Count Odd Numbers below n

oddCount :: Int -> Int 
oddCount n = n `div` 2

-- Count of positives / sum of negatives

countPositivesSumNegatives :: Maybe [Int] -> [Int]
countPositivesSumNegatives (Just []) = []
countPositivesSumNegatives (Just xs) = [length . filter (>0), sum . filter (<0)]<*>[xs]
countPositivesSumNegatives Nothing = []

-- My head is at the wrong end!

reorder :: [String] -> [String]
reorder partz 
        | not (null partz) = [last partz] ++ tail (init partz) ++ [head partz]
        | otherwise = []

-- Is it a palindrome?

-- isPalindrom :: String -> Bool
-- isPalindrom str = mk (reverse str) == mk str

-- Remove exclamation marks

removeExclamationMarks :: String -> String
removeExclamationMarks str = [ x | x <- str, x `notElem` "!"]

-- Is the string uppercase?

isUpperCase :: String -> Bool
isUpperCase str = fmap toUpper str == str

-- Count the Monkeys!

monkeyCount :: Int -> [Int]
monkeyCount x = [1..x]

-- Opposite number

opposite :: Num a => a -> a
opposite a = -a

-- Alan Partridge II - Apple Turnover

apple :: Either String Int -> String
apple (Right a) = if a^2 > 1000 then "It's hotter than the sun!!"
                  else  "Help yourself to a honeycomb Yorkie for the glovebox."
apple (Left a) = if read a ^2 > 1000 then "It's hotter than the sun!!"
                  else  "Help yourself to a honeycomb Yorkie for the glovebox."


-- Convert a Number to a String!

numberToString :: Int -> String
numberToString = show

-- Keep Hydrated!

litres :: Double -> Integer
litres d = floor $ d / 2

-- Function 1 - hello world

greet :: String
greet = "hello world!"

-- Area of a Square

round'' :: Double -> Integer -> Double
round'' num n = (fromIntegral . round $ num * f) / f
    where f = 10^n

squareArea :: Double -> Double
squareArea 0 = 0
squareArea x = round' ((2 * x / pi) ^ 2) 2

-- Basic variable assignment

a = "code"
b = "wa.rs"
name = a ++ b

-- Sum of positive

positiveSum :: [Int] -> Int
positiveSum [] = 0
positiveSum [x] | x < 0 = 0
                | otherwise = x
positiveSum (x:xs)  | x < 0 = positiveSum xs
                    | otherwise = x + positiveSum xs

-- Calculate BMI

bmi :: Float -> Float -> String  
bmi weight height | bmi <= 18.5 = "Underweight"
                  | bmi <= 25.0 = "Normal"
                  | bmi <= 30.0 = "Overweight"
                  | otherwise = "Obese"
              where bmi = weight / (height^2)

-- Third Angle of a Triangle

otherAngle :: Int -> Int -> Int
otherAngle a b = 180 - (a + b)

-- Remove First and Last Character

removeChar :: String -> String
removeChar str = init(tail str)

-- Parse nice int from char problem

getAge :: (Integral a, Read a) => String -> a
getAge (x:xs) = read [x]

-- Difference of Volumes of Cuboids

findDifference :: (Int, Int, Int) -> (Int, Int, Int) -> Int
findDifference (a,b,c) (d,e,f) = if res > 0 then res
                                else -res
                                where res = a*b*c - d*e*f

-- Function 2 - squaring an argument

square :: Integer -> Integer
square x = x * x

-- Convert boolean values to strings 'Yes' or 'No'.

boolToWord :: Bool -> String
boolToWord x | x = "Yes"
             | otherwise = "No"

-- Reversed Strings

solution :: String -> String
solution = reverse

-- Find the smallest integer in the array

findSmallestInteger :: [Int] -> Int
findSmallestInteger = minimum

-- Beginner - Reduce but Grow

grow :: [Int] -> Int
grow = product

-- Opposites Attract

inlove :: Int -> Int -> Bool
inlove a b = (odd a && even b) || (odd b && even a)

-- Simple multiplication

simpleMultiplication :: Int -> Int
simpleMultiplication n | odd n = n*9
                       | otherwise = n*8

-- The 'if' function

_if :: Bool -> a -> a -> a
_if b x y | b = x
          | otherwise = y

-- How many lightsabers do you own?

howManyLightsabersDoYouOwn :: Num a => [Char] -> a
howManyLightsabersDoYouOwn "Zach" = 18
howManyLightsabersDoYouOwn x = 0

-- Grasshopper - Debug sayHello

sayHello :: String -> String
sayHello str = "Hello, " ++ str

-- Invert

invert :: [Integer] -> [Integer]
invert = fmap (\c -> if c > 0 then -c else abs c)

-- Sum The Strings

sumStr :: String -> String -> String
sumStr a b = show (aa + bb)
        where aa = if a == "" then 0 else read a
              bb = if b == "" then 0 else read b

-- Convert number to reversed array of digits

digitize :: Int -> [Int]
digitize s = reverse $ fmap digitToInt (show s)

-- Square(n) Sum

squareSum :: [Integer] -> Integer
squareSum x = sum (fmap (^2) x)

-- Remove String Spaces

noSpace :: String -> String
noSpace = filter (/= ' ')

-- Grasshopper - Summation

summation :: Integer -> Integer 
summation n | n == 1 = 1
            | otherwise = n + summation (n-1)