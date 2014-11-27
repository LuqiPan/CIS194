{-# OPTIONS_GHC -Wall #-}

module HW04 where

{-Only 1 implementation-}
ex1 :: a -> b -> b
ex1 _ b = b

{-2 implementations-}
ex2 :: a -> a -> a
ex2 a _ = a
-- ex2 _ a = a

{-Only 1 implementation-}
ex3 :: Int -> a -> a
ex3 _ a = a

{-NOTE: 4 distinct functions inhabit this type-}
ex4 :: Bool -> a -> a -> a
ex4 _ _ a = a

{-NOTE: 4 distinct functions inhabit this type-}
ex5 :: Bool -> Bool
ex5 b = b

{-We can't produce a out of a function of a -> a-}
ex6 :: (a -> a) -> a
ex6 = error "impossible"

{-2 implementations-}
ex7 :: (a -> a) -> a -> a
ex7 f a = f a

{-
 -NOTE: infinite number of distinct functions
 -but when input is [], the output is also []
 -}
ex8 :: [a] -> [a]
ex8 as = as

{-
 -NOTE: infinite number of distinct functions
 -but when input is [], the output is also []
 -}
ex9 :: (a -> b) -> [a] -> [b]
ex9 f as = map f as

{-impossible because Maybe a may contain nothing-}
ex10 :: Maybe a -> a
ex10 (Just a) = a
ex10 Nothing = error "impossible"

{-2 implementations-}
ex11 :: a -> Maybe a
ex11 a = Just a

{-2 implementations-}
ex12 :: Maybe a -> Maybe a
ex12 ma = ma
