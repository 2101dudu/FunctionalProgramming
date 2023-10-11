module Ficha6 where


data BTree a = Empty
             | Node a (BTree a) (BTree a)
            deriving Show

--1. a)

altura :: BTree a -> Int

altura Empty = 0
altura (Node _ a b) = 1 + max (altura a) (altura b)



--1. b)

contaNodos :: BTree a -> Float

contaNodos Empty = 0
contaNodos (Node _ a b) = 1 + contaNodos a + contaNodos b

--1. c)

folhas :: BTree a -> Int

folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ a b) = folhas a + folhas b



--1. d)

prune :: Int -> BTree a -> BTree a

prune _ Empty = Empty
prune 0 _ = Empty
prune n (Node a e d) = Node a (prune (n-1) e) (prune (n-1) d)




--1. e)

path :: [Bool] -> BTree a -> [a]

path [] (Node a _ _) = [a]
path _ Empty = []
path (False:t) (Node a e _) = a : path t e
path (True:t) (Node a _ d) = a : path t d



--1. f)

mirror :: BTree a -> BTree a 

mirror Empty = Empty
mirror (Node r e d) = Node r (mirror d) (mirror e)



--1. g)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c

zipWithBT f (Node a e d) (Node a1 e1 d1) = Node (f a a1) (zipWithBT f e e1) (zipWithBT f d d1)
zipWithBT _ _ _ = Empty



--1. h)

unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)

unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) e d) = (Node a e1 d1, Node b e2 d2, Node c e3 d3)
                        where (e1,e2,e3) = unzipBT e
                              (d1,d2,d3) = unzipBT d




--2. a)

minimo :: Ord a => BTree a -> a

minimo (Node a Empty _) = a
minimo (Node a e _) = minimo e


--2. b)

semMinimo :: Ord a => BTree a -> BTree a

semMinimo (Node a Empty d) = d
semMinimo (Node a e d ) = Node a (semMinimo e) d 


--2. c)

minSmin :: Ord a => BTree a -> (a, BTree a)

minSmin (Node a Empty d) = (a,d)
minSmin (Node a e d ) = (a1, Node a a2 d)
                where (a1,a2) = minSmin e



--2. d)

remove :: Ord a => a -> BTree a -> BTree a

remove _ Empty = Empty
remove a (Node r e d) 
    | a == r = Empty
    | a < r = Node r (remove a e) d
    | a > r = Node r e (remove a d)




--3. a)

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD
            | TE
            | MEL
         deriving Show
data Classificacao = Aprov Int
                | Rep
                | Faltou
         deriving Show
type Turma = BTree Aluno


inscNum :: Numero -> Turma -> Bool

inscNum _ Empty = False
inscNum n (Node (numero,_,_,_) e d) = n == numero || inscNum n e || inscNum n d




--3. b)


inscNome :: Nome -> Turma -> Bool

inscNome _ Empty = False
inscNome a (Node (_,nome,_,_) e d) = a == nome || inscNome a e || inscNome a d



--3. c)

insere :: Ord a => (a,b) -> [(a,b)] -> [(a,b)]

insere (a,b) [] = [(a,b)]
insere (a,b) ((x,xs):t)
                | a > x = (x,xs): insere (a,b) t
                | otherwise = (a,b):(x,xs):t

iSorte :: Ord a => [(a,b)] -> [(a,b)]

iSorte [] = []
iSorte (h:t) = insere h (iSorte t) 




trabEst :: Turma -> [(Numero,Nome)]

trabEst Empty = []
trabEst (Node (numero,nome,TE,_) e d) = insere (numero,nome) (iSorte (trabEst e ++ trabEst d))
trabEst (Node _ e d) = iSorte (trabEst e ++ trabEst d)




--3. d)

nota :: Numero -> Turma -> Maybe Classificacao

nota _ Empty = Nothing
nota n (Node (numero,_,_,a) e d) 
    | n == numero = Just a
    | inscNum n e = nota n e
    | inscNum n d = nota n d
    | otherwise = Nothing




--3. e)

percFaltas :: Turma -> Float

percFaltas b = (contaFaltas b / contaNodos b) * 100


contaFaltas :: Turma -> Float

contaFaltas Empty = 0
contaFaltas (Node (_,_,_,Faltou) e d) = 1 + contaFaltas e + contaFaltas d
contaFaltas (Node _ e d) = contaFaltas e + contaFaltas d




--3. f)

mediaAprov :: Turma -> Float

mediaAprov b = somaNotas b / contaNotas b


somaNotas :: Turma -> Float

somaNotas Empty = 0
somaNotas (Node (_,_,_,Aprov a) e d) = fromIntegral a + somaNotas e + somaNotas d
somaNotas (Node _ e d) = somaNotas e + somaNotas d


contaNotas :: Turma -> Float

contaNotas Empty = 0
contaNotas (Node (_,_,_,Aprov _) e d) = 1 + contaNotas e + contaNotas d
contaNotas (Node _ e d) = contaNotas e + contaNotas d





--3. d)

aprovAv :: Turma -> Float

aprovAv b = contaNotas b / contaAvaliados b


contaAvaliados :: Turma -> Float

contaAvaliados Empty = 0
contaAvaliados (Node (_,_,_,Faltou) e d) = contaAvaliados e + contaAvaliados d
contaAvaliados (Node _ e d) = 1 +contaAvaliados e + contaAvaliados d








