import Data.List

--1
insert':: Ord a => a -> [a]->[a]
insert' x (h:t)
    | x <= h = x:h:t
    | otherwise = h: insert' x t

--2
catMaybes:: [Maybe a]->[a]
catMaybes [] = []
catMaybes (Nothing:t) = catMaybes t
catMaybes (Just a:t) = a:catMaybes t

--3
data Exp a = Const a
            | Var String
            | Mais (Exp a) (Exp a)
            | Mult (Exp a) (Exp a)
instance (Show a)=> Show (Exp a) where
    show (Const a) = show a
    show (Var s) = s
    show (Mais e d) = show e ++ " + " ++ show d
    show (Mult e d) = show e ++ " * " ++ show d

--4
sortOn:: Ord b => (a->b)->[a]->[a]
sortOn f (h:t) = inserta f h (Main.sortOn f t)
    where
        inserta _ h [] = [h]
        inserta f x (h:t)
            | f x <= f h = x:h:t
            | otherwise = h: inserta f x t

--5
--a
amplitude:: [Int]->Int
amplitude [] = 0
amplitude [h] = 0 
amplitude (h:t) = aux h h t
    where
        aux min max [] = max - min
        aux min max (h:t)
            | h < min = aux h max t
            | h > max = aux min h t
            | otherwise = aux min max t
--b
parte:: [Int]->([Int],[Int])
parte l = aux x (x,[]) (amplitude x) (length x-1)
    where
        x = sort l
        aux l (e,d) _ 0 = (e,d)
        aux l (e,d) soma n 
            | y < soma = aux l (e1,d1) y (n-1)
            | otherwise = aux l (e,d) soma (n-1)
                where
                    (e1,d1) = splitAt n l
                    y = amplitude e1 + amplitude d1

--6
data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]
            deriving Show

--a
conta:: Imagem->Int
conta (Quadrado _) = 1
conta (Mover _ i) = conta i
conta (Juntar l) = sum (map conta l)

--b
--apaga:: Imagem->IO Imagem
--apaga i = do    x <- randomRIO (1,conta i)
--                return (aux i x)
--    where
aux (Quadrado _) 1 = Juntar []
aux (Mover e i) x = Mover e (aux i x)
aux (Juntar (h:t)) x 
ex :: Imagem
ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5),Quadrado 4,Mover (4,3) (Quadrado 2)])

unlines' :: [String] -> String
unlines' [] = []
unlines' [h] = h
unlines' (x:y:t) = x ++ "\n" ++ (unlines' (y:t))