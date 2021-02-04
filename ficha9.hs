
import Data.Char
--import System.Random
--1
--1a
bingo:: IO ()
bingo = do  putStrLn "Bem vindo ao Bingo\n"
            putStrLn "Pressione para Começar\n"
            getChar
            mostra (sorteio [1..90]) []

sorteio:: [Int]->[Int]
sorteio (x:xs) = c:(sorteio h++ts)
    where
        (h,c:ts) = splitAt (randomRIO (0,length (x:xs)-1)) (x:xs)

mostra:: [Int]->[Int]->IO()
mostra [] _ = do putStrLn "Fim do Sorteio"
mostra (h:t) s = do putStrLn (show h++"\n")
                    putStrLn "Pressione qualquer tecla...\n"
                    getChar 
                    mostra t (h:s)

--1b
mastermind::IO ()
mastermind = do putStrLn "Bem vindo ao Mastermind\n"
                putStrLn "Pressione qualquer tecla para começar\n"
                let chave = (listaAl 4)
                tenta 15 chave

listaAl:: Int->String 
listaAl 0 = []
listaAl n = (randomRIO ('0','9')):listaAl (n-1)

tenta:: Int->String ->IO ()
tenta 0 chave = do  putStrLn "Ficou sem tentativas!\n"
                    putStrLn ("A chave é " ++ chave)
tenta n chave = do  input <- getjogada
                    if (input == chave) then (putStrLn "Ganhou!")
                    else 
                        putStrLn ("Número de posições certas: " ++ show (poscertas input chave) ++ "\n" ++ "Número de posições erradas: " ++ show ((poserradas
                         input chave) - (poscertas input chave)) ++"\n") 
                    tenta (n-1) chave

poscertas::String->String->Int
poscertas [] _ = 0
poscertas _ [] = 0
poscertas (x:xs) (y:ys)
    | x == y = 1 + poscertas xs ys
    | otherwise = poscertas xs ys

poserradas::String ->String ->Int 
poserradas [] _ = 0
poserradas _ [] = 0
poserradas (h:t) l
    | elem h l = 1 + poserradas t l
    | otherwise = poserradas t l

getjogada:: IO String 
getjogada = do  putStrLn "Digite 4 Dígitos\n"
                jogada <- getLine 
                if ((length jogada)==4 && testajogada jogada) then return jogada
                else do getjogada

testajogada:: String->Bool
testajogada [] = True
testajogada (h:t) = h > '0' && h < '9' && testajogada t

--2
--2a
data Aposta = Ap [Int] (Int,Int)
valida::Aposta->Bool 
valida (Ap l (e1,e2)) = (e1 /= e2) && (e1<= 9 && e1>=1 && e2>=1 && e2<=9) && try l
    where
        try (x:xs) = (x<=50 && x>=1) && (all (\l -> l==x) xs) && try xs


