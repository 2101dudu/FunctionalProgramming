module Ficha9 where

import System.Random
import Data.Char



--1. a)


bingo :: IO ()
bingo = do acumularN []


acumularN :: [Int] -> IO ()
acumularN l 
        | length l == 90 = print "Feito"
        | otherwise = do v <- randomRIO (1,90)
                         if (v `elem` l) then acumularN l else print v
                         getChar
                         acumularN (v:l)




--1. b)

mastermind :: IO ()
mastermind = do let i = 1000 :: Int
                let f = 9999 :: Int
                numeroR <- randomRIO (i,f)
                let numrString = show numeroR
                putStrLn "Introduza um número com 4 dígitos"
                a <- getLine
                mainAux a numrString
                



mainAux :: String -> String -> IO()
mainAux a b
        | verificaPos a b == (4,0) = print "Feito"
        | otherwise = do putStrLn (show (fst (verificaPos a b)) ++ " número(s) estão na posiçao certa e " ++ (show (snd(verificaPos a b))) ++ " número(s) presentes, mas na posiçao errada")
                         putStrLn "Tente novamente:"
                         c <- getLine
                         mainAux c b




verificaPos :: String -> String -> (Int,Int)

verificaPos _ [] = (0,0)
verificaPos [] _ = (0,0)
verificaPos (h:t) (x:xs)
        | h == x = (a+1,b)
        | h `elem` xs = (a,b+1)
        | otherwise = (a,b)
        where (a,b) = verificaPos t xs







--2. a)

data Aposta = Ap [Int] (Int,Int)

valida :: Aposta -> Bool
valida (Ap l (a,b)) = numDiferentes l && (length (filter (<=50) l) == 5) && (length (filter (>=1) l) == 5) && a <= 9 && b <= 9 && a >= 1 && b >= 1 && a /=       b


numDiferentes :: [Int] -> Bool

numDiferentes [] = True
numDiferentes (h:t)
        | elem h t = False
        | otherwise = numDiferentes t





--2. b)

comuns :: Aposta -> Aposta -> (Int,Int)

comuns l t = (numerosComuns l t, estrelasComuns l t)

 

numerosComuns :: Aposta -> Aposta -> Int

numerosComuns (Ap [] _) _ = 0
numerosComuns (Ap (h:t) (a,b)) (Ap l (c,d))
        | elem h l = numerosComuns (Ap t (a,b)) (Ap l (c,d)) + 1
        | otherwise = numerosComuns (Ap t (a,b)) (Ap l (c,d))


estrelasComuns :: Aposta -> Aposta -> Int

estrelasComuns (Ap _ (a,b)) (Ap _ (c,d))
        | (a == c || a == d) && (b == c || b == d) = 2
        | (a == c || a == d) || (b == c || b == d) = 1
        | otherwise = 0



--2. c) i.

instance Eq Aposta where
     a1 == a2 = comuns a1 a2 == (5,2)



--2. c) ii.

premio :: Aposta -> Aposta -> Maybe Int
premio ap1 ap2
        | n == (5,2) = Just 1
        | n == (5,1) = Just 2
        | n == (5,0) = Just 3
        | n == (4,2) = Just 4
        | n == (4,1) = Just 5
        | n == (4,0) = Just 6
        | n == (3,2) = Just 7
        | n == (2,2) = Just 8
        | n == (3,1) = Just 9
        | n == (3,0) = Just 10
        | n == (1,2) = Just 11
        | n == (2,1) = Just 12
        | n == (2,0) = Just 13
        | otherwise = Nothing
        where n = comuns ap1 ap2


--2. d) i.

leAposta :: IO Aposta

leAposta = do putStrLn "Introduza 5 numeros da sua aposta"
              n1 <- getLine
              let numero1 = stringToInt n1
              n2 <- getLine
              let numero2 = stringToInt n2
              n3 <- getLine
              let numero3 = stringToInt n3
              n4 <- getLine
              let numero4 = stringToInt n4
              n5 <- getLine
              let numero5 = stringToInt n5
              putStrLn "Introduza 2 estrelas da sua aposta"
              e1 <- getLine
              let estrela1 = stringToInt e1
              e2 <- getLine
              let estrela2 = stringToInt e2
              let numeros = [numero1,numero2,numero3,numero4,numero5]
              let estrelas = (estrela1,estrela2)
              if valida (Ap numeros estrelas) then return (Ap numeros estrelas) else do putStrLn "Aposta Inválida, tente novamente:"
                                                                                        leAposta

 

stringToInt :: String -> Int
stringToInt [] = 0
stringToInt (h:t) = (digitToInt h) * (10 ^ (length (h:t) - 1)) + stringToInt t







              

--2. d) ii.

joga :: Aposta -> IO ()
joga chave = do aposta <- leAposta
                let n = premio aposta chave
                print ("O seu premio e: " ++ (show n))


--2. e)

geraChave :: IO Aposta
geraChave = do let a = 1  :: Int
               let b = 50 :: Int
               let c = 9  :: Int
               n1 <- randomRIO (a,b)
               n2 <- randomRIO (a,b)
               n3 <- randomRIO (a,b)
               n4 <- randomRIO (a,b)
               n5 <- randomRIO (a,b)
               e1 <- randomRIO (a,c)
               e2 <- randomRIO (a,c)
               let numeros = [n1,n2,n3,n4,n5]
               let estrelas = (e1,e2)
               if valida (Ap numeros estrelas) then return (Ap numeros estrelas) else geraChave




main2 :: IO ()
main2 = do ch <- geraChave
           joga ch


main :: Int -> IO ()
main n = do (Ap num est) <- geraChave
            (Ap num2 est2) <- geraChave
            if premio (Ap num est) (Ap num2 est2) == Just 1 then print ("Terminado. As chaves eram: Ap " ++ show num ++ "" ++ show est ++ " e Ap " ++ show num2 ++ show est2)  else do print ("Tentativa numero: " ++ show n)
                                                                                                                                                                                       main (n+1)     