--1.

import Data.Char

digitAlpha :: String -> (String,String)

--digitAlpha l = (filter (isAlpha) l, filter (isDigit) l)

digitAlpha = foldl dA ("","")
    where dA :: (String,String) -> Char -> (String,String)
          dA (alphas,digits) c
                | isAlpha c = (alphas ++ [c],digits)
                | isDigit c = (alphas, digits ++ [c])
                | otherwise = (alphas,digits)



--2.

nzp :: [Int] -> (Int,Int,Int)

--nzp l = (length (filter (<0) l),length (filter (==0) l), length (filter (>0) l))

{-
nzp = foldl nzpA (0,0,0) 
    where nzpA :: (Int,Int,Int) -> Int -> (Int,Int,Int)
          nzpA (a,b,c) n
            | n < 0 = (a+1,b,c)
            | n > 0 = (a,b,c+1)
            | otherwise = (a,b+1,c
-}

nzp = foldl (\ (a,b,c) n -> if (n<0) then (a+1,b,c) else if (n>0) then (a,b,c+1) else (a,b+1,c)) (0,0,0)




--3.

divsMod :: Integral a => a -> a -> (a, a)

divsMod a b = (divs a b, mods a b)


divs :: Integral a => a -> a -> a
divs a b
    | a - b < 0 = 0
    | otherwise = 1 + divs (a-b) b

mods :: Integral a => a -> a -> a
mods a b 
    | a - b < 0 = a 
    | otherwise = mods (a-b) b






--4.

{-fromsDigits :: [Int] -> Int
fromsDigits [] = 0
fromsDigits (h:t) = h*10^(length t) + fromsDigits t-}





--8.

devolveNumeros :: Int -> [Int]

devolveNumeros 20 = []
devolveNumeros x 
    | x > 0 && mod x 2 == 0 && mod x 3 == 0 = x : devolveNumeros (x+1)
    | otherwise = devolveNumeros (x+1)


