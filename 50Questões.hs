module Questões where 

import Data.Char (ord)

--1.

enumsFromTo :: Int -> Int -> [Int]

enumsFromTo a b
    | a < b = a : (enumsFromTo (a+1) b)
    | otherwise = [b]


--2.

enumsFromThenTo :: Int -> Int -> Int -> [Int]

enumsFromThenTo a b c 
    | a > c && b >= a || a < c && b < a = []
    | otherwise = a : enumsFromThenTo b (2*b - a) c




--3. 

(+++) :: [a] -> [a] -> [a]

(+++) [] [] = []
(+++) (x:xs) [] = x:(+++) xs []
(+++) [] (x:xs) = x:(+++) xs []
(+++) (h:t) (x:xs) = h:(+++) t (x:xs)


--4.

(!!!) :: [a] -> Int -> a

(!!!) l a
    | a == length l - 1 = last l
    | otherwise = (!!!) (init l) a 



--5.

reverter :: [a] -> [a]

reverter [] = []
reverter (h:t) = reverter t ++ [h]



--6.

taker :: Int -> [a] -> [a]

taker _ [] = []
taker n (h:t)
    | n <= 0 = []
    | otherwise = h :  take (n-1) t


--7.

droper :: Int -> [a] -> [a]

droper _ [] = []
droper n (h:t) 
    | n <= 0 = (h:t)
    | otherwise = droper (n-1) t



--8.

zipper :: [a] -> [b] -> [(a,b)]

zipper _ [] = []
zipper [] _ = []
zipper (h:t) (x:xs) = (h,x) : (zipper t xs)



--9.

replicater :: Int -> a -> [a]

replicater n x
    | n <= 0 = []
    | otherwise = x : replicater (n-1) x



--10.

intersperse :: a -> [a] -> [a]

intersperse _ [] = []
intersperse _ [a] = [a]
intersperse a (h:t) = h:a:intersperse a t


--11.

group :: Eq a => [a] -> [[a]]

group [] = []
group (h:t) = insere h (group t)


insere :: Eq a => a -> [[a]] -> [[a]]

insere a [] = [[a]]
insere a (h:t)
    | elem a h = (a:h):t
    | otherwise = [a] : (h:t)


--12.

concater :: [[a]] -> [a]

concater [] = []
concater (h:t) = h ++ concater t


--13.

inits :: [a] -> [[a]]

inits [] = []
inits l = inits (init l) ++ [l]


--14.

tails :: [a] -> [[a]]

tails [] = [[]]
tails l = [l] ++ tails (tail l)



--15.

heads :: [[a]] -> [a]

heads [] = []
heads ([]:t) = heads t
heads (h:t) = [head h] ++ heads t


--16.

total :: [[a]] -> Int

total [] = 0
total ([]:t) = total t
total (h:t) = length h + total t



--17.

fun :: [(a,b,c)] -> [(a,c)]

fun [] = []
fun ((a,_,c):t) = [(a,c)] ++ fun t


--18.

cola :: [(String,b,c)] -> String

cola [] = []
cola ((a,_,_):t) = a ++ cola t


--19.

idade :: Int -> Int -> [(String,Int)] -> [String]

idade _ _ [] = []
idade a b ((c,d):t)
    | a - b >= d = [c] ++ idade a b t 
    | otherwise = idade a b t


--20.

powerEnumFrom :: Int -> Int -> [Int]

powerEnumFrom n m
    | m <= 0 = []
    | otherwise = powerEnumFrom n (m-1) ++ [n^(m-1)]




--21.

isPrime :: Int -> Bool

isPrime n = n >= 2 && primeCheck n 2


primeCheck :: Int -> Int -> Bool

primeCheck n m
    | m*m > n = True
    | mod n m == 0 = False
    | otherwise = primeCheck n (m+1)





--22.

isPrefixOf :: Eq a => [a] -> [a] -> Bool

isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (h:t) (x:xs)
    | length (h:t) <= length (x:xs) && h == x = isPrefixOf t xs
    | otherwise = False



--23.

isSuffixOf :: Eq a => [a] -> [a] -> Bool

isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf (h:t) (x:xs)
    | last (h:t) == last (x:xs) = isSuffixOf (init(h:t)) (init(x:xs))
    | otherwise = False




--24.

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool

isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (h:t) (x:xs)
    | h == x = isSubsequenceOf t xs
    | elem h (x:xs) = isSubsequenceOf (h:t) xs
    | otherwise = False





--25.

elemIndices :: Eq a => a -> [a] -> [Int]

elemIndices _ [] = []
elemIndices a (h:t) 
    | a == h = 0 : map (+1) (elemIndices a t)
    | otherwise = map (+1) (elemIndices a t)


--26.

nub :: Eq a => [a] -> [a]

nub [] = []
nub (h:t) 
    | elem h t = nub t
    | otherwise = h : nub t



--27.

delete :: Eq a => a -> [a] -> [a]

delete _ [] = []
delete a (h:t)
    | a == h = t
    | otherwise = h : (delete a t)





--28.

(\\) :: Eq a => [a] -> [a] -> [a]

(\\) [] _ = []
(\\) l [] = l
(\\) (h:t) (x:xs) = delete x ((\\) (h:t) xs) 



--29.

union :: Eq a => [a] -> [a] -> [a]

union [] _ = []
union _ [] = []
union l (h:t)
    | elem h l = union l t
    | otherwise = l ++ [h] ++ union l t



--30.

intersect :: Eq a => [a] -> [a] -> [a]

intersect [] _ = []
intersect _ [] = []
intersect l (h:t)
    | elem h l = filter (==h) l ++ intersect (filter (/=h) l) t
    | otherwise = intersect l t


--31.

insert :: Ord a => a -> [a] -> [a]

insert n [] = [n]
insert n (h:t)
    | h <= n && n <= head t = h:n:t
    | otherwise = [h] ++ insert n t



--32.

unwordss :: [String] -> String

unwordss [] = []
unwordss (h:t) = h ++ (if null t then "" else " ") ++ unwordss t


--33.

unliness :: [String] -> String

unliness [] = []
unliness (h:t) = h ++ "\n" ++ unliness t



--34.

pMaior :: Ord a => [a] -> Int

pMaior (h:t)
    | null (filter (>h) (h:t)) = 0
    | otherwise = 1 + pMaior t



--35.

lookups :: Eq a => a -> [(a,b)] -> Maybe b

lookups _ [] = Nothing
lookups n ((x,xs):t)
    | n == x = Just xs
    | otherwise = lookups n t




--36.

preCrescente :: Ord a => [a] -> [a]

preCrescente [] = []
preCrescente [x] = [x]
preCrescente (h:s:t) 
    | h < s = [h] ++ preCrescente (s:t)
    | otherwise = [h]





--37.

iSort :: Ord a => [a] -> [a]

iSort [] = []    
iSort (h:t) = insert h (iSort t)



--38.

menor :: String -> String -> Bool

menor [] _ = True
menor _ [] = False 
menor (h:t) (x:xs)
    | h == x = menor t xs
    | h < x = True
    | otherwise = False




--39.

elemMSet :: Eq a => a -> [(a,Int)] -> Bool

elemMSet _ [] = False
elemMSet n ((a,_):t)
    | n == a = True
    | otherwise = elemMSet n t



--40.

converteMSet :: [(a,Int)] -> [a]

converteMSet [] = []
converteMSet ((_,0):t) = converteMSet t
converteMSet ((a,b):t) = [a] ++ converteMSet ((a,b-1):t)



--41. 

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]

insereMSet n [] = [(n,1)]
insereMSet n ((a,b):t)
    | n == a = (a,b+1):t
    | otherwise = (a,b):insereMSet n t




--42.

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]

removeMSet _ [] = []
removeMSet n ((a,b):t)
    | n == a = if b > 1 then (a,b-1):t else t
    | otherwise = (a,b) : removeMSet n t 





--43.

constroiMSet :: Ord a => [a] -> [(a,Int)]

constroiMSet [] = []
constroiMSet [a] = [(a,1)]  
constroiMSet (h:t) = (h,length (filter (==h) (h:t))):constroiMSet(filter (/=h) (h:t))



--44.

partitionEithers :: [Either a b] -> ([a],[b])

partitionEithers [] = ([],[])
partitionEithers (Left a: t) = ([a] ++ fst (partitionEithers t), snd (partitionEithers t))
partitionEithers (Right a: t) = (fst (partitionEithers t), [a] ++ snd (partitionEithers t))



--45.

catMaybes :: [Maybe a] -> [a]

catMaybes [] = []
catMaybes (Just a:t) = [a] ++ catMaybes t
catMaybes (Nothing:t) = catMaybes t



--46.
data Movimento = Norte | Sul | Este | Oeste
                deriving (Show,Eq)


caminho :: (Int,Int) -> (Int,Int) -> [Movimento]

caminho (0,b) (0,d)
    | b < d = prodLista (d-b) Norte
    | otherwise = prodLista (b-d) Sul

caminho (a,0) (c,0)
    | a < c = prodLista (c-a) Este
    | otherwise = prodLista (a-c) Oeste

caminho (a,b) (c,d) = (caminho (a,0) (c,0)) ++ (caminho (0,b) (0,d))



prodLista :: Int -> a -> [a]

prodLista 0 _ = []
prodLista n a = [a] ++ prodLista (n-1) a




--47.

hasLoops :: (Int,Int) -> [Movimento] -> Bool

hasLoops _ [] = False
hasLoops a b = a == caminhoV2 a b || hasLoops a (init b)


caminhoV2 :: (Int,Int) -> [Movimento] -> (Int,Int)

caminhoV2 p [] = p
caminhoV2 (a,b) (Norte:t) = caminhoV2 (a,b+1) t
caminhoV2 (a,b) (Sul:t) = caminhoV2 (a,b-1) t
caminhoV2 (a,b) (Este:t) = caminhoV2 (a+1,b) t
caminhoV2 (a,b) (Oeste:t) = caminhoV2 (a-1,b) t




--48.

type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int

contaQuadrados [] = 0
contaQuadrados ((Rect (a,b) (c,d)):t)
    | abs (a-c) == abs (b-d) = 1 + contaQuadrados t
    | otherwise = contaQuadrados t


--49.

areaTotal :: [Rectangulo] -> Float

areaTotal [] = 0
areaTotal ((Rect (a,b) (c,d)):t) = abs(a-c)*abs(b-d) + areaTotal t


--50.

data Equipamento = Bom | Razoavel | Avariado
        deriving Show

naoReparar :: [Equipamento] -> Int

naoReparar [] = 0
naoReparar (Bom:t) = 1 + naoReparar t
naoReparar (Razoavel:t) = 1 + naoReparar t
naoReparar (Avariado:t) = naoReparar t