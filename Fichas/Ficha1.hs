module Ficha1 where

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use even" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Char ()
import Data.Functor.Classes (Eq1)
import Language.Haskell.TH (TySynEqnQ)

-- 1. D)

multiplo :: Int -> Int -> Bool

multiplo a b = if mod a b == 0 then True else False





-- 1. E)

truncaImpar :: [a] -> [a]

truncaImpar l =
    if mod (length l) 2 == 0 then l
    else tail l





-- 1. F)

max2 :: Int -> Int -> Int

max2 a b = if a > b then a else b


max3 :: Int -> Int -> Int -> Int

max3 a b c = if max2 a b > c then max2 a b else c





-- 2. a)

nRaizes :: Float -> Float -> Float -> Int

nRaizes a b c
  | b^2 - 4 * a * c == 0 = 1
  | b^2 - 4 * a * c > 0 = 2
  | otherwise = 0





-- 2. b)

raizes :: Float -> Float -> Float -> [Float]

raizes a b c
  | nRaizes a b c == 2 = [((-b) - sqrt(b^2 - 4*a*c)) / 2*a ,  ((-b)+ sqrt(b^2 - 4*a*c)) / 2*a]
  | nRaizes a b c == 1 = [ (-b) / 2*a]
  | nRaizes a b c == 0 = []
  




-- 3. a)

type Hora = (Int,Int)


check :: Hora -> Bool

check (x,y) 
  | x<0 || 23<x = False
  | y<0 || 59<y = False
  | otherwise = True




-- 3. b)

dep :: Hora -> Hora -> Bool

dep (a,b) (c,d)
  | check (a,b) && check (c,d) && a > c = True
  | check (a,b) && check (c,d) && a < c = False
  | check (a,b) && check (c,d) && a == c && b < d = False
  | check (a,b) && check (c,d) && a == c && b > d = True
  | check (a,b) && check (c,d) && a == c && b == d = error "Horas Iguais"
  | otherwise = error "Horas Inválidas"





-- 3. c)

conv :: Hora -> Int   

conv (a,b) 
  | check (a,b) = a*60 + b
  | otherwise = error "Horas Inválidas"





-- 3. d)

convh :: Int -> Hora

convh a 
  | a >= 0 = (div a 60, mod a 60) 
  | otherwise = error "Minuto(s) inválido(s)"
  




-- 3. e)

dif :: Hora -> Hora -> Int

dif (a,b) (c,d)
  | check (a,b) && check (c,d) && a == c = abs (b - d)
  | check (a,b) && check (c,d) && b == d = abs (a - c) * 60 
  | check (a,b) && check (c,d) =  abs (a - c) * 60 + abs (b - d)
  | otherwise = error "Horas Inválidas"





-- 3. f)

adi :: Hora -> Int -> Hora

adi (a,b) c
  | (b + c) > 59 = (a + div (b + c) 60, mod (b + c) 60)
  | (b + c) < 60 = (a,(b + c))





-- 4. a)
  
data HORA = H Int Int 
  deriving (Show,Eq)

checkHORA :: HORA -> Bool

checkHORA (H h m)
  | h>=0 && 24>=h && m>=0 && 60>=m = True
  | otherwise = False





-- 4. b)

maiorHORA :: HORA -> HORA -> Bool

maiorHORA (H h m) (H a b)
  | h > a = True
  | h < a = False
  | h == a && m > b = True
  | h == a && m < b = False
  | otherwise = error "Horas Iguais"






-- 4. c)

convHORA :: HORA -> Int

convHORA (H h m) = h*60 + m 



-- 5. a)

data Semaforo = Verde 
              | Amarelo 
              | Vermelho 
  deriving (Show,Eq)



next :: Semaforo -> Semaforo

next c 
  | c == Verde = Amarelo
  | c == Amarelo = Vermelho
  | c == Vermelho = Verde





-- 5. b)

stop :: Semaforo -> Bool

stop c 
  | c == Vermelho = True
  | otherwise = False




-- 5. c)

safe :: Semaforo -> Semaforo -> Bool

safe a b 
  | a == Vermelho && b == Vermelho = True
  | a == Vermelho && b == Verde || a == Verde && b == Vermelho = True
  | a == Vermelho && b == Amarelo || a == Amarelo && b == Vermelho = True
  | otherwise = False







-- 6. a)

data Ponto = Cartesiano Double Double
           | Polar Double Double
  deriving (Show,Eq)


posx :: Ponto -> Double

posx (Cartesiano a b) = abs (a)
posx (Polar a b) = abs (a * cos(b))






-- 6. b)

posy :: Ponto -> Double

posy (Cartesiano a b) = abs (b)
posy (Polar a b) = abs (a * sin(b)) 







-- 6. c)

raio :: Ponto -> Double

raio (Cartesiano a b) = sqrt(a^2 + b^2)
raio (Polar a b) = a






-- 6. d)

angulo :: Ponto -> Double

angulo (Cartesiano a b) = atan (b/a)
angulo (Polar a b) = b




-- 6. e)

dist :: Ponto -> Ponto -> Double


dist (Polar a b) (Polar c d) = sqrt((a * cos b - c * cos d)^2 + (a * sin b - c * sin d)^2)
dist (Cartesiano a b) (Polar c d) = sqrt((a - c * cos d)^2 + (b - c * sin d)^2)
dist (Cartesiano a b) (Cartesiano c d) = sqrt((a - c)^2 + (b - d)^2)  
dist (Polar a b) (Cartesiano c d) = sqrt((c - a * cos b)^2 + (d - a * sin b)^2)






-- 7. a)

data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
  deriving (Show,Eq)





poligono :: Figura -> Bool

poligono (Circulo a b) 
  | b > 0 = True
  | otherwise = False

poligono (Rectangulo a b) 
  | posx a /= posx b && posy a /= posy b = True
  | otherwise = False 

poligono (Triangulo a b c) 
  | a == b = False
  | a == c = False
  | b == c = False
  | otherwise = True





-- 7. b)

vertices :: Figura -> [Ponto]

vertices (Circulo a b) = []
vertices (Rectangulo a b) = [a,b]
vertices (Triangulo a b c) = [a,b,c]







-- 7. c)

area :: Figura -> Double

area (Triangulo p1 p2 p3) 
  | poligono (Triangulo p1 p2 p3) == False =
  let a = dist p1 p2
      b = dist p2 p3
      c = dist p3 p1
      s = (a+b+c) / 2 
  in sqrt (s*(s-a)*(s-b)*(s-c))  
  | otherwise = error "Não é um triângulo"

area (Circulo c r) 
  | poligono (Circulo c r) = pi*r^2
  | otherwise = error "Não é um círculo"

area (Rectangulo p1 p2) 
  | poligono (Rectangulo p1 p2) = abs (posx p1 - posx p2) * abs (posy p1 - posy p2)
  | otherwise = error "Não é um retângulo"








-- 7. d)

perimetro :: Figura -> Double

perimetro (Triangulo p1 p2 p3)
  | poligono (Triangulo p1 p2 p3) =
  let a = dist p1 p2
      b = dist p2 p3
      c = dist p3 p1
   in a+b+c
  | otherwise = error "Não é poligono"

perimetro (Circulo c r) 
  | poligono (Circulo c r) = 2 * pi * r
  | otherwise = error "Não é poligono"  

perimetro (Rectangulo a b) 
  | poligono (Rectangulo a b) = abs(posx a - posx b) * 2 + abs(posy a - posy b)*2
  | otherwise = error "Não é poligono"






-- 8. a)



  

