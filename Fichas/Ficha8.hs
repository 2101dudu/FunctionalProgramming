module Ficha8 where
import Data.List
import System.Random

--1. a)

data Frac = F Integer Integer

normaliza :: Frac -> Frac
normaliza (F x y) = F (div (abs x) (mdc x y)) (div (abs y) (mdc x y))


mdc :: Integer -> Integer -> Integer
mdc x y 
    | x == y = y
    | x > y  = mdc (x-y) y
    | x < y  = mdc x (y-x)




--2. b)

instance Eq Frac where
    f1 == f2 = (a==x) && (b==y)
        where (F a b) = normaliza f1
              (F x y) = normaliza f2


instance Ord Frac where
    f1 <= f2 = a*y <= x*b
     where (F a b) = normaliza f1
           (F x y) = normaliza f2


instance Show Frac where
    show (F a b) = (show a) ++ "/" ++ (show b) 

instance Num Frac where  
        (F a b) + (F x y) = F (a*y + b*x) (b*y)
        (F a b) * (F x y) = F (a*x) (b*y)
        negate (F a b) = F (-a) b
        abs (F a b) = F (abs a) (abs b)
        signum (F a b) = F ((signum a) * (signum b)) 1
        fromInteger n = F n 1


--1. f)

maioresDobro :: Frac -> [Frac] -> [Frac]
maioresDobro f l = filter (> 2*f) l


--2. a)



data Exp a = Const a
    | Simetrico (Exp a)
    | Mais (Exp a) (Exp a)
    | Menos (Exp a) (Exp a)
    | Mult (Exp a) (Exp a)


instance Show a => Show (Exp a) where
    show (Const a) = show a
    show (Mais a b) = show a ++ " + " ++ show b
    show (Menos a b) = show a ++ " - " ++ show b
    show (Mult a b) = show a ++ " * " ++ show b
    show (Simetrico a) = "-(" ++ show a ++ ")"


--2. b)


calcula :: Num a => Exp a -> a
calcula (Const a) = a
calcula (Simetrico a) = - (calcula a)
calcula (Mais a b) = calcula a + calcula b
calcula (Menos a b) = calcula a - calcula b
calcula (Mult a b) = calcula a * calcula b

instance (Eq a,Num a) => Eq (Exp a) where
    a == b = calcula a == calcula b



--2. c)


instance (Num a) => Num (Exp a) where
    a + b = Const (calcula a + calcula b) 
    a * b = Const (calcula a * calcula b) 
    negate a = Const (- calcula a)
    signum a = Const (signum (calcula a))
    abs a = Const (abs (calcula a))
    fromInteger n = Const (fromInteger n)



    



type Nome = String
type Telefone = Integer
data Agenda = Vazia | Nodo (Nome,[Telefone]) Agenda Agenda


instance Show Agenda where
    show Vazia = " "
    show (Nodo (nome,tel) aE aD) = show aE ++ "--" ++ nome ++ "  Numeros: "++ transformaTelefone tel ++ "--" ++ show aD




transformaTelefone :: [Telefone] -> String
transformaTelefone [] = ""
transformaTelefone [h] = show h
transformaTelefone (h:t) = show h ++ "/" ++ transformaTelefone t











randomSel :: Int -> [a] -> IO [a]
randomSel n l = do lRan <- aux n (length l - 1) []
                   return (listaLista lRan l)




aux :: Int -> Int -> [Int] -> IO [Int]  
aux 0 _ lRan = return lRan
aux n l lRan = do ran <- randomRIO (0,l :: Int)
                  if n > l+1 then aux (l+1) l lRan else if length lRan == l+1 then return lRan else if elem ran lRan then aux n l lRan else aux (n-1) l (ran:lRan)
                 
                   
listaLista :: [Int] -> [a] -> [a]
listaLista [] _ = []
listaLista (h:t) l = (!!) l h : listaLista t l 
                              











data RTree a = R a [RTree a]
    deriving (Show)
type Dictionary = [RTree (Char, Maybe String)]


insere :: String -> String -> Dictionary -> Dictionary

insere [] _ _ = []

insere [x] descricao [] = [R (x,Just descricao) []]
insere (h:t) descricao [] = [R (h, Nothing) (insere t descricao [])]


insere [x] descricao ((R (letra,d) l):xs) 
    | x == letra = ((R (letra,Just descricao) l):xs)
    | otherwise = [R (letra,d) (insere [x] descricao xs)]
insere (h:t) descricao ((R (letra,d) l):xs)
    | h == letra = (R (letra,d) (insere t descricao l):xs)
    | otherwise = insere (h:t) descricao xs










type Mat = [[Int]]

triSup :: Mat -> Bool
triSup [] = True
triSup (h:t) = head (map (primeirosIndices) (map (devolveIndices) (h:t))) < head (tail (map (primeirosIndices) (map (devolveIndices) (h:t)))) && triSup t



devolveIndices :: [Int] -> [Int]
devolveIndices [] = []
devolveIndices (h:t)
    | h == 0 = 0 : (map (+1) (devolveIndices t))
    | otherwise = (map (+1) (devolveIndices t))



primeirosIndices :: [Int] -> [Int]

primeirosIndices [] = []
primeirosIndices [x] = [x]
primeirosIndices (h:s:t)
    | h /= 0 = []
    | h+1 == s = h:primeirosIndices (s:t)
    | otherwise = [h]




jogo :: Int -> (Int,Int) -> IO ()

jogo n (a,b) = do ran <- randomRIO (a,b :: Int) 
                  let lRan = replicate n ran
                  print lRan
                  










type MSet a = [(a,Int)]

calcular :: MSet a -> ([a],Int)
calcular l = foldl (func) ([],0) l
        where func :: ([a],Int) -> (a,Int) -> ([a],Int)
              func (a,b) (x,y) = (x : a , b + y)



    


