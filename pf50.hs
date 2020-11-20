
--1
enumFromTo':: Int->Int->[Int]
enumFromTo' f t 
      | f /= t = f:enumFromTo' (f+1) t
      | otherwise = [t]

--2
enumFromThenTo':: Int->Int->Int->[Int]
enumFromThenTo' f the to
      | f<=to = f: enumFromThenTo' the (f+(f-the)) to
      | otherwise = []

--3
juntaListas:: [a]->[a]->[a]
juntaListas [] l = l
juntaListas (x:xs) l2 = x:juntaListas xs l2

--4
elem'::[a]->Int->a
elem' (h:_) 0 = h
elem' (_:t) n = elem' t (n-1)

--5
reverse'::[a]->[a]
reverse' [] = []
reverse' l = auxacc l []
      where
            auxacc:: [a]->[a]->[a]
            auxacc [] acc = acc
            auxacc (h:t) acc = auxacc t (h:acc)

--6
take':: Int->[a]->[a]
take' 0 _ = []
take' _ [] = []
take' n (h:t) = h:take' (n-1) t

--7
drop':: Int->[a]->[a]
drop' 0 l = l
drop' _ [] = []
drop' n (_:t) = drop' (n-1) t

--8
zip':: [a]->[b]->[(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y): zip' xs ys

--9
elem'':: Eq a=> a->[a]->Bool
elem'' _ [] = False
elem'' h1 (h2:t)
      | h1 == h2 = True
      | otherwise = elem'' h1 t

--10
replicate'::Int->a->[a]
replicate' 0 _ = []
replicate' n x = x:replicate' (n-1) x

--11
intersperse:: a->[a]->[a]
intersperse _ [] = []
intersperse _ [h] = [h]
intersperse x (h:t) = h:x:intersperse x t

--12
agrupa:: Eq a => [a]->[[a]]
agrupa [] = []
agrupa (h:t) = agrupaacc [h] t
      where
            agrupaacc acc [] = [acc]
            agrupaacc (x:xs) (h:t)
                  | x == h = agrupaacc (h:x:xs) t
                  | otherwise = (x:xs):agrupaacc [h] t

agrupa':: Eq a => [a]->[[a]]
agrupa' [] = []
agrupa' l = p:agrupa' (drop' (length p) l)
      where
            p = aux l
                  where
                        aux:: Eq a => [a]->[a]
                        aux [x] = [x]
                        aux (h1:h2:t) 
                              | h1==h2 = h1:aux (h2:t)
                              | otherwise = [h1]

--13
concat'::[[a]]->[a]
concat' [] = []
concat' ([]:t) = concat' t
concat' ((x:xs):t) = x:concat'(xs:t)

--14
{-
inits':: [a]->[[a]]
inits' [] = []
inits' l = aux l (length l)
      where
            aux _ 0 = [[]]
            aux l n = aux l (n-1) ++ [l]
            -}
inits'::[a]->[[a]]
inits' [] = [[]]
inits' l = inits' (initx l) ++ [l]
      where
            initx [] = []
            initx [_] = []
            initx (h:t) = h:initx t

--15
tails':: [a]->[[a]]
tails' [] = [[]]
tails' (h:t) = (h:t):tails' t

--16
isPrefixOf':: Eq a => [a]->[a]->Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' l1 l2
      | l1 == l2 = True
      | otherwise = isPrefixOf' l1 (init l2)

--17
isSuffixOf:: Eq a => [a]->[a]->Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf l1 l2
      | l1 == l2 = True
      | otherwise = isSuffixOf l1 (tail l2)

--18
isSubsequenceOf:: Eq a => [a]->[a]->Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (h1:t1) (h2:t2)
      | h1 == h2 = isSubsequenceOf t1 t2
      | otherwise = isSubsequenceOf (h1:t1) t2

--19
elemIndices:: Eq a => a -> [a] -> [Int]
elemIndices x l = accaux 0 x l
      where
            accaux _ _ [] = []
            accaux n x (h:t)
                  | x == h = n:accaux (n+1) x t
                  | otherwise = accaux (n+1) x t

--elemIndices:: Eq a => a -> [a] -> [Int]
--elemIndices n l = [y | (x,y) <- zip' l [0..] , x == n]

--20
nub:: Eq a => [a]->[a]
nub [] = []
nub (h:t) = h:nub (nubaux h t)
      where
            nubaux x (h:t)
                  | h == x = nubaux x t
                  | otherwise = h: nubaux x t 

--21
delete:: Eq a => a->[a]->[a]
delete _ [] = []
delete n (x:xs)
      | n == x = xs
      | otherwise = x: delete n xs

--22
remove:: Eq a => [a]->[a]->[a]
remove [] _ = []
remove l [] = l
remove l1 (h:t) = remove (deleteaux h l1) t
      where 
            deleteaux _ [] = []
            deleteaux x (h:t)
                  | x == h = t
                  | otherwise = h:deleteaux x t

--23
union:: Eq a => [a]->[a]->[a]
union [] l2 = l2
union l1 [] = l1
--union l1 (h:t) = union (unionaux l1 h) t
--      where
--            unionaux [] x = [x]
--            unionaux (h:t) x
--                  | h == x = h:t
--                  | otherwise = h:unionaux t x
union l1 l2 = l1 ++ removeaux l2 l1
      where
            removeaux [] _ = []
            removeaux l [] = l
            removeaux l1 (h:t) = remove (deleteaux h l1) t
                  where
                        deleteaux _ [] = []
                        deleteaux x (h:t)
                              | x==h = t
                              | otherwise = h:deleteaux x t

--24
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' _ [] = []
intersect' l1 (h:t) = intersect' (intersectaux l1 h) t
      where
            intersectaux [] _ = []
            intersectaux (h:t) x
                  | h == x = h:intersectaux t x
                  | otherwise = intersectaux t x

--25
insert' :: Ord a => a->[a]->[a]
insert' _ [] = []
insert' x (h:t) 
      | x<=h = x:(h:t)
      | otherwise = h: insert' x t

--26
unwords':: [String]->String
unwords' [] = []
unwords' [x] = x 
unwords' (h:t) = h ++ " " ++ unwords' t 

--27
unlines':: [String]->String
unlines' [] = []
unlines' (h:t) = h ++ "\n" ++ unlines' t

--28
pMaior:: Ord a => [a] ->Int
pMaior [] = error "There aren't any elements"
pMaior (h:t) = pMaioraux (zip' t [1..]) (h,0)
      where 
            pMaioraux [] (_,i) = i
            pMaioraux ((h,i):t) (a,b)
                  | h <= a = pMaioraux t (a,b)
                  | otherwise = pMaioraux t (h,i) 
--pMaior l = snd (pmaioraux l)
--      where
--            pmaioraux [h] =  (h,0) 
--            pmaioraux (h:t)
--                  | h <= m = (m,p+1)
--                  | otherwise = (h,0)
--                        where (m,p) = pmaioraux t

--29
temRepetidos:: Eq a => [a]->Bool
temRepetidos (h1:h2:t) = aux h1 (h2:t) || temRepetidos (h2:t)
      where
            aux h1 (h2:t)
                  | h1 == h2 = True
                  | otherwise = aux h1 t
temRepetidos _ = False

--30
algarismos:: [Char]->[Char]
algarismos (h:t)
      | h<='9' || h>='0' = h: algarismos t
      | otherwise =  algarismos t

--31
posImpares:: [a]->[a]
posImpares [] = []
posImpares (_:t) = auxposImpares t
      where
            auxposImpares [] = []
            auxposImpares (h:t) = h: posImpares t
posImpares':: [a]->[a]
posImpares' [] = []
posImpares' [_] = []
posImpares' (_:h2:hs) = h2:posImpares' hs

--32
posPares:: [a]->[a]
posPares [] = []
posPares (h:t) = h: auxposPares t
      where
            auxposPares [] = []
            auxposPares (_:t) = posPares t

posPares' :: [a] -> [a]
posPares' [] = []
posPares' [c] = [c]
posPares' (h:x:xs) = h:posPares' xs

--33
isSorted:: Ord a => [a]->Bool
isSorted (h1:h2:t)
      | h1>h2 = False
      | otherwise = isSorted (h2:t)

--34
iSort':: Ord a => [a]->[a]
iSort' [] = []
iSort' (h:t) = insert' h (iSort' t)
      where
            insert' x (h:t)
                  | x <= h = x:(h:t)
                  | otherwise = h: insert' x t

--35
menor:: String->String->Bool
menor [] _ = True
menor _ [] = False
menor (h1:t1) (h2:t2)
      | h1<h2 = True
      | otherwise = menor t1 t2

--36
elemMSet:: Eq a => a->[(a,Int)]->Bool
elemMSet _ [] = False
elemMSet x ((c,_):t) 
      | x == c = True
      | otherwise = elemMSet x t

--37
lengthMSet:: [(a,Int)]->Int
lengthMSet [] = 0
lengthMSet ((_,n):t) = n + lengthMSet t

--38
converteMSet:: [(a,Int)]->[a]
converteMSet ((c,n):t)
      | n == 1 = c: converteMSet t
      | otherwise = c: converteMSet((c,n-1):t)

--39
insereMSet:: Eq a => a->[(a,Int)]->[(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((c,e):t)
      | x == c = (c,e+1):t
      | otherwise = (c,e):insereMSet x t

--40
removeMSet:: Eq a => a->[(a,Int)]->[(a,Int)]
removeMSet _ [] = []
removeMSet x ((c,e):t)
      | x == c && e>1 = (c,e-1):t
      | x== c && e == 1 = t
      | otherwise = (c,e): removeMSet x t

--41
constroiMSet:: Ord a => [a]->[(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = insereMSet h (constroiMSet t)

--42
partitionsEithers:: [Either a b]->([a],[b])
partitionsEithers [] = ([],[])
partitionsEithers (h:t) = case h of
      Left a -> (a:x,y)
      Right b -> (x,b:y)
      where
            (x,y) = partitionsEithers t

--43
catMaybes:: [Maybe a]->[a]
catMaybes [] = []
catMaybes ((Nothing):t) = catMaybes t
catMaybes ((Just a):t) = a:catMaybes t

--44
data Movimento = Norte | Este | Sul | Oeste deriving Show

posicao:: (Int, Int)-> [Movimento]->(Int,Int)
posicao p [] = p
posicao (x,y) (h:t) = case h of
      Norte -> posicao (x,y+1) t
      Sul -> posicao (x,y-1) t
      Este -> posicao (x+1, y) t
      Oeste -> posicao (x-1,y) t

--45
caminho:: (Int,Int)->(Int,Int)->[Movimento]
caminho (x1,y1) (x2,y2)
      | y2>y1 = Norte:caminho (x1,y1+1) (x2,y2)
      | y2<y1 = Sul:caminho (x1,y1-1) (x2,y2)
      | x2>x1 = Este:caminho (x1+1,y1) (x2,y2)
      | x2<x1 = Oeste:caminho (x1-1,y1) (x2,y2)
      | otherwise = []

--46
vertical:: [Movimento]->Bool
vertical [] = True
vertical (Norte:_) = False
vertical (Sul:_) = False
vertical (_:t) = vertical t

--47
data Posicao = Pos Int Int deriving Show

maisCentral:: [Posicao]->Posicao
maisCentral [p] = p 
maisCentral ((Pos x1 y1):(Pos x2 y2):t)
      | (x1^2 + y1^2) >= (x2^2 +y2^2) = maisCentral ((Pos x1 y1):t)
      | otherwise = maisCentral (Pos x1 y1:t)

--48
vizinhos:: Posicao->[Posicao]->[Posicao]
vizinhos _ [] = []
vizinhos (Pos x y) (Pos x1 y1:t)
      | (x1 == x+1 && y == y1) || (x1==x-1 && y == y1) || (x1==x && y == y1+1) || (x1==x && y==y1-1) = Pos x1 y1:vizinhos (Pos x y) t
      | otherwise = vizinhos (Pos x y) t

--49
mesmaOrdenada:: [Posicao]->Bool
mesmaOrdenada [] = True
mesmaOrdenada [_] = True
mesmaOrdenada ((Pos x1 y1):(Pos _ y2):t)
      | y1 == y2 = mesmaOrdenada ((Pos x1 y1):t)
      | otherwise = False

--50
data Semafaro = Verde | Amarelo | Vermelho deriving Show
interseccaoOK:: [Semafaro]->Bool
interseccaoOK [] = True
interseccaoOK (Vermelho:t) = interseccaoOK t
interseccaoOK (_:t) = interseccaoNOK t
      where
            interseccaoNOK [] = True
            interseccaoNOK (Vermelho:t) = interseccaoNOK t
            interseccaoNOK _ = False

temRepetidoseu :: Eq a => [a] -> Bool
temRepetidoseu [] = False
temRepetidoseu (x:xs) 
      | (x == head xs) || (temRepetidoseu (xs)) = True
      | otherwise = False