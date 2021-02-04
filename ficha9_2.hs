import Data.Char(digitToInt)

--2
--2a
data Aposta = Ap [Int] (Int,Int)
valida::Aposta->Bool 
valida (Ap l (e1,e2)) = (e1 /= e2) && (e1<= 9 && e1>=1 && e2>=1 && e2<=9) && try l
    where
        try [] = True 
        try (x:xs) = (x<=50 && x>=1) && (all (\l -> l/=x) xs) && try xs

--2b
comuns:: Aposta->Aposta->(Int,Int)
comuns (Ap l1 (e1,e2)) (Ap l2 (e3,e4))
    | valida (Ap l1 (e1,e2)) && valida (Ap l2 (e3,e4)) = (x,y) 
    | otherwise = error "Apostas não válidas"
        where
            x = va l1 l2
            y = va [e1,e2] [e3,e4]
va :: [Int]->[Int]->Int
va _ [] = 0
va (x:xs) l2
    | elem x l2 = 1 + va xs l2
    | otherwise = va xs l2

--2c
--i
instance Eq Aposta where
    a1 == a2 = comuns a1 a2 == (5,2)

--ii
premio:: Aposta->Aposta->Maybe Int
premio a1 a2
    | x == 5 = if (y==2) then (Just 1) else if (y==1) then (Just 2) else Just 3
    | x == 4 = if (y==2) then (Just 4) else if (y==1) then (Just 5) else Just 6 
    | x == 3 = if (y==2) then (Just 7) else if (y==1) then (Just 9) else Just 10
    | x == 2 = if (y==2) then (Just 8) else if (y==1) then (Just 12) else Just 13
    | x == 1 = if (y==2) then (Just 11) else Nothing 
    | otherwise = Nothing
    where
        (x,y) = comuns a1 a2

--2d
--i
leAposta::IO Aposta
leAposta = do   putStrLn "Introduza os números"
                x <- numeros []
                y <- estrelas []
                return (Ap x (head y,last y))
                where
                    l = estrelas []

estrelas:: [Int]->IO [Int]
estrelas l
    | length l == 2 = return l
    | otherwise = do  input <-getLine 
                      let x = read input :: Int
                      if (x>9 || x<1) 
                      then 
                            do putStrLn "Número inválido"
                               estrelas l
                        else if (elem x l)
                            then
                                do putStrLn "Número já inserido"
                                   estrelas l
                        else
                            estrelas (x:l)

numeros:: [Int]->IO [Int]
numeros l
    | length l == 5 = return l
    | otherwise = do  input <-getLine
                      let x = read input :: Int
                      if (x>50 || x<1) 
                      then 
                            do putStrLn "Número inválido"
                               numeros l
                        else if (elem x l)
                            then
                                do putStrLn "Número já inserido"
                                   numeros l
                        else
                            numeros (x:l)

--ii
joga:: Aposta -> IO ()
joga a1 = do x <- leAposta
             let y = premio a1 x
             case y of
                 Nothing -> putStrLn "Nenhum prémio"
                 Just a -> putStrLn ("Prémio " ++ show a)

--e
geranumeros:: [Int]->[Int]->[Int]
geranumeros acc sample
    | acc == 5 = acc
    | otherwise = (geranumeros (c:acc) (x ++ t))
        where
            (x,c:t) = splitAt (randomRIO(0,length sample-1)) sample

geraestrelas:: [Int]->(Int,Int)
geraestrelas sample = (x,y)
    where
        (a,x:b) = splitAt (randomRIO(0,length sample - 1)) sample
        (c,y:d) = splitAt (randomRIO(0,length (a++b)-1)) (a++b)


geraChave:: IO Aposta
geraChave = do return (Ap (geranumeros [] [1..50]) (geraestrelas [1..9]))