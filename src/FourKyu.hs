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

-- Five Fundamental Monads

-- class Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b

-- data Identity a = Identity a
--   deriving (Show, Eq)

-- data Maybe a = Nothing | Just a
--   deriving (Show, Eq)

-- data State s a = State {runState :: s -> (a, s)}

-- data Reader s a = Reader {runReader :: s -> a }

-- data Writer w a = Writer {runWriter :: (w, a)}

-- instance Monad Identity where
--   return = Identity
--   (Identity v) >>= f = f v

-- instance Monad Maybe where
--   return = Just
--   Nothing >>= f = Nothing
--   (Just v) >>= f = f v

-- instance Monad (State s) where
--   return x = State $ \s -> (x,s) 
--   (State g) >>= f = State $ \s -> let (a, newState) = g s  
--                                       (State h) = f a  
--                                   in  h newState 

-- instance Monad (Reader s) where
--   return x = Reader $ \_ -> x 
--   (Reader g) >>= f = Reader $ \w -> runReader (f (g w)) w 

-- instance Monoid w => Monad (Writer w) where
--   return x = Writer (mempty, x)
--   (Writer (s, v)) >>= f = let (Writer (s', v')) = f v
--                           in Writer (s `mappend` s', v')

fusc :: Integer -> Integer
fusc n = fuscRec n 1 0

fuscRec :: Integer -> Integer -> Integer -> Integer
fuscRec 0 a b = b
fuscRec 1 a b = a + b
fuscRec n a b | even n    = fuscRec (n `div` 2) (a + b) b
              | otherwise = fuscRec (n `div` 2) a (a + b)

