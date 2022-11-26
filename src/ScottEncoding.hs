{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module ScottEncoding where

import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }
newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

toPair :: SPair a b -> (a,b)
toPair (SPair f) = f (,)
fromPair :: (a,b) -> SPair a b
fromPair (a, b) = SPair (\f -> f a b)
fst :: SPair a b -> a
fst (SPair f) = f const
snd :: SPair a b -> b
snd (SPair f) = f (const id)
swap :: SPair a b -> SPair b a
swap (SPair f) = SPair (f . flip)
curry :: (SPair a b -> c) -> (a -> b -> c)
curry f a b = f (SPair (\x -> x a b))
uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry a b = a (fst b) (snd b)

cons :: a -> SList a -> SList a
cons a b = SList (\_ f -> f a b)

nil' :: SList a
nil' = SList const

toMaybe :: SMaybe a -> Maybe a
toMaybe (SMaybe f) = f Nothing Just 
fromMaybe :: Maybe a -> SMaybe a
fromMaybe f = SMaybe (\b x -> maybe b x f)
isJust :: SMaybe a -> Bool
isJust (SMaybe f) = f False (const True)
isNothing :: SMaybe a -> Bool
isNothing = not . isJust
catMaybes :: SList (SMaybe a) -> SList a
catMaybes (SList f) = f nil' (\(SMaybe x) l -> x id cons (catMaybes l))

toEither :: SEither a b -> Either a b
toEither (SEither a) = a Left Right
fromEither :: Either a b -> SEither a b
fromEither f = SEither (\a x -> either a x f)
isLeft :: SEither a b -> Bool
isLeft (SEither f) = f (const True) (const False)
isRight :: SEither a b -> Bool
isRight = not . isLeft
partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition (SList f) = f (SPair (\g -> g nil' nil'))
 (\(SEither g) l ->
    let SPair p = partition l
    in p (\a b -> SPair (\h -> h(g cons (const id) a) (g (const id) cons b)))
    )

toList :: SList a -> [a]
toList (SList a) = a [] (\a l -> a : toList l) 
fromList :: [a] -> SList a
fromList l = SList (\b f -> case l of 
                        [] -> b
                        (a:r) -> f a (fromList r))
concat :: SList a -> SList a -> SList a
concat (SList f) g = f g (\a l -> cons a (concat l g))
null :: SList a -> Bool
null (SList f) = f True (const (const False))
length :: SList a -> Int
length (SList f) = f 0 (\_ l -> length l + 1)
map :: (a -> b) -> SList a -> SList b
map f (SList g) = SList (\z c -> g z (\a l -> c (f a) (map f l)))
zip :: SList a -> SList b -> SList (SPair a b)
zip (SList f) (SList g) = f nil' (\a l -> g nil' (\b m -> cons (SPair (\h -> h a b)) (zip l m))) 
foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f b l = foldr (\b g x -> g (f x b)) id l b
foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f b (SList g) = g b (\a l -> f a (foldr f b l))  
take :: Int -> SList a -> SList a
take n (SList f) = f nil' (\a l -> if n > 0 then cons a (take (n-1) l) else nil')
