{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Use ==" #-}
{-# HLINT ignore "Use infix" #-}
import GHC.Unit.Database (DbModule(DbModuleVar))
import Data.Char (chr, ord)

-- 2. a)

dobros :: [Float] -> [Float]

dobros [] = []
dobros (h:t) = [h*2] ++ dobros t





-- 2. b)

numOcorre :: Char -> String -> Int

numOcorre _ [] = 0
numOcorre a (h:t) 
    | elem a [h] = 1 + numOcorre a t 
    | otherwise = numOcorre a t





-- 2. c)

positivos :: [Int] -> Bool

positivos [] = True
positivos (h:t) 
    | h > 0 = positivos t
    | otherwise = False





--2. d)

soPos :: [Int] -> [Int]

soPos [] = []
soPos (h:t) 
    | h <= 0 = soPos t
    | otherwise = [h] ++ soPos t





--2. e)

somaNeg :: [Int] -> Int

somaNeg [] = 0
somaNeg (h:t) 
    | h < 0 = h + somaNeg t
    | otherwise = somaNeg t





--2. f)

tresUlt :: [a] -> [a]

tresUlt [] =[]
tresUlt (h:t) 
    | length (h:t) <= 3 = h:t
    | otherwise = tresUlt t






--2. g) 

segundos :: [(a,b)] -> [b]

segundos [] = []
segundos ((x,y):xs) = [y] ++ segundos xs






--2. h)

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool 

nosPrimeiros a [] = False
nosPrimeiros a ((x,y):xs) 
    | a == x = True 
    | otherwise = nosPrimeiros a xs







--2. i)

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)

sumTriplos [] = (0,0,0)
sumTriplos [(a,b,c)] = (a,b,c)
sumTriplos [(a,b,c),(x,y,z)] = (a+x,b+y,c+z)
sumTriplos ((a,b,c):(x,y,z):xs) = sumTriplos ((a+x, b+y, c+z):xs)








--3. a)

soDigitos :: [Char] -> [[Char]]

soDigitos [] = []
soDigitos (h:t)
    | ord h <= 57 && ord h >= 48 = [[h]] ++ soDigitos t
    | otherwise = soDigitos t





--3. b)

minusculas :: [Char] -> Int

minusculas [] = 0
minusculas (h:t) 
    | ord h <= 122 && ord h >= 97 = 1 + minusculas t
    | otherwise = minusculas t





--3. c)

nums :: String -> [Int]

nums [] = []
nums (h:t) = ord h : nums t







--4. a)

type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta :: Int -> Polinomio -> Int

conta _ [] = 0
conta n (h:t) 
    | elem n h = 1 + conta n t
    | otherwise = conta n t








--4. b)

grau :: Polinomio -> Int 

grau [] = 0
grau [h] = snd h
grau (h:t) 
    | snd h > grau t = snd h 
    | otherwise = grau t 








--4. c)

selgrau :: Int -> Polinomio -> Polinomio

selgrau _ [] = []
selgrau a ((b,c):t)
    | a == c = (b,c) : selgrau a t
    | otherwise = selgrau a t







--4. d) 

deriv :: Polinomio -> Polinomio

deriv [] = []
deriv ((a,b):t) = (a * fromIntegral b, b-1) : deriv t






--4. e) 

calcula :: Float -> Polinomio -> Float 

calcula _ [] = 0
calcula a ((b,c):t) = b*(a^c) + calcula a t 





--4. f) 

simp :: Polinomio -> Polinomio 

simp [] = []
simp ((a,b):t) 
    | a == 0 = simp t
    | otherwise = (a,b) : simp t






--4. g)

mult :: Monomio -> Polinomio -> Polinomio

mult _ [] = []
mult (a,b) ((h,t):xs) = (a*h,b+t) : mult (a,b) xs





--4. h)

normaliza :: Polinomio -> Polinomio

normaliza [] = []
normaliza [x] = [x]
normaliza ((a,b):t)   
    | b == snd (head (normaliza t)) = (a + fst (head t), b) : t
    | otherwise = t 

















--4. j)

produto :: Polinomio -> Polinomio -> Polinomio

produto ((x,y):t) ((a,b):xs) = (x*a,y+b) : produto ((x,y):t) xs