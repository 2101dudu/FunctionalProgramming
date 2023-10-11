{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}



type Hora = (Int,Int)


check :: Hora -> Bool

check (x,y) 
  | x<0 || 23<x = False
  | y<0 || 59<y = False
  | otherwise = True





-- 3. b)

dep :: HORA -> HORA -> Bool

dep (H a b) (H c d)
  | check (a, b) && check (c,d) && a > c = True
  | check (a, b) && check (c,d) && a < c = False
  | check (a, b) && check (c,d) && a == c && b < d = False
  | check (a, b) && check (c,d) && a == c && b > d = True
  | check (a, b) && check (c,d) && a == c && b == d = error "Horas Iguais"
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

dif :: HORA -> HORA -> Int

dif (H h1 m1) (H h2 m2)
    | h1 > h2 = (h1*60 + m1) - (h2*60  + m2)
    | h1 < h2 = (h2*60 + m2) - (h1*60  + m1)
    | h1 == h2 && m1 > m2 = m1 - m2
    | h1 == h2 && m1 < m2 = m2 - m1
    | otherwise = error "Horas Inválidas"





-- 3. f)

adi :: Hora -> Int -> Hora

adi (a,b) c
  | (b + c) > 59 = (a + div (b + c) 60, mod (b + c) 60)
  | (b + c) < 60 = (a,b + c)





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
  | h < a = True
  | h > a = False
  | h == a && m < b = True
  | h == a && m > b = False
  | otherwise = error "Horas Iguais"






-- 4. c)

convHORA :: HORA -> Int

convHORA (H h m) = h*60 + m 





-----------------------------------------------------------------------------------------------------------------------------------------------------------



--Ficha 3 
--1. a)

type Etapa = (HORA,HORA)
type Viagem = [Etapa]

etapaCheck :: Etapa -> Bool

etapaCheck (H a b, H x y)
    | check (a,b) && check (x,y) && maiorHORA (H a b) (H x y) = True
    | otherwise = False

    {-check (x,y) 
  | x<0 || 23<x = False
  | y<0 || 59<y = False
  | otherwise = True-}





--1. b)

viagemCheck :: Viagem -> Bool

viagemCheck [xs] = etapaCheck xs 
viagemCheck ((a, b):(c, d):xs)
    | etapaCheck (a,b) && maiorHORA b c && viagemCheck ((c,d):xs) = True    
    | otherwise = False





--1. c)

horaViagem :: Viagem -> Etapa

horaViagem h 
    | viagemCheck h = (fst (head h), snd (last h))
    | otherwise = error "Viagem Inválida"





--1. e)

tempoEspera :: Viagem -> Int 

tempoEspera [t] = 0 
tempoEspera ((a,b):(c,d):xs)
    | viagemCheck ((a,b):(c,d):xs) = dif b c + tempoEspera ((c,d):xs) 
    | otherwise = error "Viagem Inválida"





--1. d) 

tempoViagem :: Viagem -> Int

tempoViagem h
    | viagemCheck h = dif (fst (head h)) (snd (last h)) - tempoEspera h




--1. f) 

tempoTotalViagem :: Viagem -> Int

tempoTotalViagem h
    | viagemCheck h = tempoViagem h + tempoEspera h





----------------------
-- 6. a)

data Ponto = Cartesiano Double Double
           | Polar Double Double
  deriving (Show,Eq)


posx :: Ponto -> Double

posx (Cartesiano a b) = (a)
posx (Polar a b) = abs (a * cos(b))






-- 6. b)

posy :: Ponto -> Double

posy (Cartesiano a b) = (b)
posy (Polar a b) = abs (a * sin(b)) 







-- 6. c)

raio :: Ponto -> Double

raio (Cartesiano a b) = sqrt(a^2 + b^2)
raio (Polar a b) = a






-- 6. d)

angulo :: Ponto -> Double

angulo (Cartesiano a b) = atan (b/a)
angulo (Polar _ b) = b




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

poligono (Circulo _ b) 
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

vertices (Circulo _ _) = []
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


----------------------

--2. a)

type Poligonal = [Ponto]

distPont :: Poligonal -> Double 

distPont [] = 0
distPont [_] = 0
distPont (h:a:t) = dist h a + distPont (a:t)





--2. b) 

testefechado :: Poligonal -> Bool

testefechado [] = error "Não é uma linha"
testefechado [_] = error "Não é uma linha"
testefechado (h:t)
    | dist h (last t) == 0 = True
    | otherwise = False





--2. c)


--triangula :: Poligonal -> [Figura]
















--3. a) 

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
  deriving (Show,Eq)


type Nome = String
type Agenda = [(Nome, [Contacto])]



acrescEmail :: Nome -> String -> Agenda -> Agenda

acrescEmail a b [] = [(a, [Email b])]
acrescEmail a b ((c,d):t)
    | a == c = ((c,Email b : d):t)
    | otherwise = (c,d) : acrescEmail a b t 




--3. b)

checkifemail :: [Contacto] -> [String]

checkifemail [] = []
checkifemail (Email h:t) = h : checkifemail t
checkifemail (_:t) = checkifemail t


verEmails :: Nome -> Agenda -> Maybe [String]

verEmails _ [] = Nothing
verEmails a ((b,c):t)
  | a == b = Just (checkifemail c)
  | otherwise = verEmails a t







-- 3. c)

checkifNumb :: [Contacto] -> [Integer]

checkifNumb [] = []
checkifNumb (Tlm h:t) = h : checkifNumb t
checkifNumb (Casa h:t) = h : checkifNumb t
checkifNumb (Trab h:t) = h : checkifNumb t
checkifNumb (_:t) = checkifNumb t






--3. d)


checkifCasa :: [Contacto] -> Maybe Integer

checkifCasa [] = Nothing
checkifCasa (Casa h:_) = Just h
checkifCasa (_:t) = checkifCasa t

casa :: Nome -> Agenda -> Maybe Integer

casa _ [] = Nothing
casa a ((b,c):t)
  | a == b = checkifCasa c  
  | otherwise = casa a t


  





--4. a)

type Dia = Int
type Mes = Int
type Ano = Int
data Data = D Dia Mes Ano
    deriving Show
type TabDN = [(Nome,Data)]


procura :: Nome -> TabDN -> Maybe Data

procura _ [] = Nothing
procura a ((c,d):t)
  | a == c = Just d
  | otherwise = procura a t




--4. b)

calculoTempo :: Data -> Data -> Int

calculoTempo (D d1 m1 a1) (D d2 m2 a2)
  | a2 - a1 > 0 = error "Datas Negativas"
  | otherwise = div ((a1 - a2) * 365 + (m1 - m2) * 30 + (d1 - d2)) 365



idade :: Data -> Nome -> TabDN -> Maybe Int

idade _ _ [] = Nothing
idade a b ((c,l):t)
  | b == c = Just (calculoTempo a l)
  | otherwise = idade a b t






--4. c)

anterior :: Data -> Data -> Bool

anterior (D d1 m1 a1) (D d2 m2 a2)
  | a1 < a2 = True
  | a1 == a2 && m1 < m2 = True
  | a1 == a2 && m1 == m2 && d1 < d2 = True
  | otherwise = False






--4. d)

{-ordena :: TabDN -> TabDN

ordena [] = []
ordena [(a,b)] = [(a,b)]

ordena [(a,b),(c,d)] 
  | anterior b d = [(a,b),(c,d)]
  | otherwise = [(c,d),(a,b)]

ordena ((a,b):(c,d):t) =  ordena (ordena [(a,b),(c,d)] ++ t)-}














--5. a)

data Movimento = Credito Float | Debito Float
    deriving Show   

data Extracto = Ext Float [(Data, String, Movimento)]
    deriving Show

  


extValor :: Extracto -> Float -> [Movimento]

extValor (Ext _ []) _ = []

extValor (Ext a ((_,_,Debito c):t)) n
  | c > n = [Debito c] ++ extValor (Ext a t) n
  | otherwise = extValor (Ext a t) n

extValor (Ext a ((_,_,Credito c):t)) n
  | c > n = [Credito c] ++ extValor (Ext a t) n
  | otherwise = extValor (Ext a t) n






--5. b)

filtro :: Extracto -> [String] -> [(Data,Movimento)]

filtro (Ext _ []) _ = []

filtro (Ext k ((a,b,Debito c):t)) g
  | elem b g = (a,Debito c) : filtro (Ext k t) g
  | otherwise = filtro (Ext k t) g

filtro (Ext k ((a,b,Credito c):t)) g
  | elem b g = (a,Credito c) : filtro (Ext k t) g
  | otherwise = filtro (Ext k t) g







--5. c)

intcredeb :: Extracto -> Float

intcredeb (Ext _ []) = 0
intcredeb (Ext k ((_,_,Debito c):t)) = 0 + intcredeb (Ext k t) 
intcredeb (Ext k ((_,_,Credito c):t)) = c + intcredeb (Ext k t) 

intcredeb1 :: Extracto -> Float

intcredeb1 (Ext _ []) = 0
intcredeb1 (Ext k ((_,_,Credito c):t)) = 0 + intcredeb1 (Ext k t) 
intcredeb1 (Ext k ((_,_,Debito c):t)) = c + intcredeb1 (Ext k t) 


creDeb :: Extracto -> (Float,Float)

creDeb (Ext _ []) = (0,0)
creDeb (Ext k (h:t)) = (intcredeb (Ext k (h:t)),intcredeb1 (Ext k (h:t)))







--5. d)

saldo :: Extracto -> Float

saldo (Ext a h) = a - fst (creDeb(Ext a h)) - snd (creDeb(Ext a h))
