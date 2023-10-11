module Ficha5 where

import Data.List()

--1. a)

anys :: (a -> Bool) -> [a] -> Bool

anys _ [] = False
anys f (h:t) = f h || (anys f t)


--1. b)

zipWiths :: (a->b->c) -> [a] -> [b] -> [c]

zipWiths f (x:xs) (h:t) = f x h : zipWiths f xs t
zipWiths _ _ _ = []


--1. c)

takeWhiles :: (a->Bool) -> [a] -> [a]

takeWhiles _ [] = []
takeWhiles f (h:t)
    | f h = h : takeWhiles f t
    | otherwise = []


--1. d)

dropWhiles :: (a->Bool) -> [a] -> [a]

dropWhiles _ [] = []
dropWhiles f (h:t)
    | f h = dropWhiles f t
    | otherwise = (h:t)


--1. e)

spans :: (a-> Bool) -> [a] -> ([a],[a])

spans _ [] = ([],[])
spans f (h:t)
    | f h = (h:x,y)
    | otherwise = ([],h:t)
        where (x,y) = spans f t

        
--1. f)

deleteBys :: (a -> a -> Bool) -> a -> [a] -> [a]

deleteBys _ _ [] = []
deleteBys f a (h:t)
    | f a h = t
    | otherwise = h : deleteBys f a t


--1. g)

sortOns :: Ord b => (a -> b) -> [a] -> [a]

sortOns _ [] = []
sortOns f (h:t) = insere h (sortOns f t)
                where insere a [] = [a]
                      insere a (x:xs)
                        | f a < f x = a:x:xs
                        | otherwise = x: insere a xs





--2. a)

type Polinomio = [Monomio]
type Monomio = (Float,Int)


selgrau :: Int -> Polinomio -> Polinomio

selgrau n l = filter (\ (_,b) -> b == n) l




--2. b)

conta :: Int -> Polinomio -> Int


