
import Data.List

--1
en::[a]->Int->a
en (h:t) 0 = h
en (h:t) x = en t (x-1)

--2
data Movimento = Norte | Sul | Este | Oeste deriving Show
posicao:: (Int,Int)->[Movimento]->(Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (h:t) = case h of
    Norte -> posicao (x,y+1) t
    Sul -> posicao (x,y-1) t
    Este -> posicao (x+1,y) t
    Oeste -> posicao (x-1,y) t

--3
any':: (a->Bool)->[a]->Bool
any' f [] = False 
any' f (h:t)
    | f h = True
    | otherwise = any' f t

--4
type Mat a = [[a]]
triSup:: (Num a,Eq a) => Mat a -> Bool 
triSup m = aux m 0 0
    where
        aux [] _ _= True
        aux (h:t) 0 x = aux t (x+1) (x+1)
        aux ((h:t1):t) n x
            | h == 0 = aux (t1:t) (n-1) x
            | otherwise = False 

--5
movimenta:: IO (Int,Int)
movimenta = aux (0,0)
    where
        aux (x,y) = do  a<-getChar 
                        if (a=='N') then aux (x,y+1)
                        else if (a=='S') then aux (x,y-1)
                        else if (a=='E') then aux (x+1,y)
                        else if (a=='O') then aux (x-1,y)
                        else return (x,y)

--6
data Imagem = Quadrado Int
    | Mover (Int,Int) Imagem
    | Juntar [Imagem]

--a
vazia:: Imagem->Bool 
vazia (Quadrado _) = False 
vazia (Mover _ i) = vazia i
vazia (Juntar []) = True
vazia (Juntar i) = all vazia i

--b
maior:: Imagem->Maybe Int
maior (Quadrado n) = Just n
maior (Mover _ i) = maior i
maior (Juntar imgs)
    | null imgs = Nothing 
    | otherwise = maximum' (filter (/= Nothing) (map maior imgs))
        where
            maximum' [] = Nothing 
            maximum' l = maximum l

--c
instance Eq Imagem where
    i1 == i2 = null ((quadPos i1 (0,0)) \\ (quadPos i2 (0,0)))
        where
            quadPos (Quadrado n) pos = [(n,pos)]
            quadPos (Mover (a,b) i) (x,y) = quadPos i (x+a,y+b)
            quadPos (Juntar i) pos = concatMap (\x->quadPos x pos) i 