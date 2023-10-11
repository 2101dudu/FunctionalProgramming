module Ficha7 where

import Data.Char

--1. a)

data ExpInt = Const Int
        | Simetrico ExpInt
        | Mais ExpInt ExpInt
        | Menos ExpInt ExpInt
        | Mult ExpInt ExpInt


calcula :: ExpInt -> Int

calcula (Const a) = a
calcula (Simetrico a) = - (calcula a)
calcula (Mais a b) = calcula a + calcula b
calcula (Menos a b) = calcula a - calcula b
calcula (Mult a b) = calcula a * calcula b




--1. b)

infixa :: ExpInt -> String 

infixa (Const a) = show a
infixa (Simetrico a) = "- " ++ infixa a 
infixa (Mais a b) = "(" ++ infixa a ++ " + " ++ infixa b ++ ")"
infixa (Menos a b) = "(" ++ infixa a ++ " - " ++ infixa b ++ ")"
infixa (Mult a b) = "(" ++ infixa a ++ " * " ++ infixa b ++ ")"




--1. c)

posfixa :: ExpInt -> String

posfixa (Const a) = show a ++ " "
posfixa (Simetrico a) = '-' : posfixa a
posfixa (Mais a b) = posfixa a ++ posfixa b ++ "+ "
posfixa (Menos a b) = posfixa a ++ posfixa b ++ "- "
posfixa (Mult a b) = posfixa a ++ posfixa b ++ "* "





--2. a)

data RTree a = R a [RTree a]
        deriving (Show)


soma :: Num a => RTree a -> a

soma (R a []) = a
soma (R a b) = a + sum (map (soma) b)




--2. b)

altura :: RTree a -> Int 

altura (R _ []) = 1
altura (R _ b) = 1 + maximum ( map (altura) b)



--2. c)

prune :: Int -> RTree a -> RTree a 

prune 0 (R a _) = R a [] 
prune n (R a b) = R a (map (prune (n-1)) b)




--2. d)

mirror :: RTree a -> RTree a

mirror (R a b) = R a (map (mirror) (reverse b))




--2. e)

postorder :: RTree a -> [a]

postorder (R a []) = [a]
postorder (R a b) = (concatMap (postorder) b) ++ [a]




--3. a)

data LTree a = Tip a | Fork (LTree a) (LTree a)



ltSum :: Num a => LTree a -> a

ltSum (Tip a) = a
ltSum (Fork a b) = ltSum a + ltSum b 



--3. b)

listaLT :: LTree a -> [a]

listaLT (Tip a) = [a]
listaLT (Fork a b) = listaLT a ++ listaLT b




--3. c)

ltHeight :: LTree a -> Int

ltHeight (Tip _) = 0
ltHeight (Fork a b) = 1 + max (ltHeight a) (ltHeight b)



--4. a)

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

