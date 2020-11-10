
--1
enumfromto:: Int->Int->[Int]
enumfromto f t
    | f == t = [t]
    | f < t = f: enumfromto (f+1) t
    | otherwise = []

--2
enumfromthento:: Int->Int->Int->[Int]
enumfromthento f t to
    | f<=t && t<=to = f: enumfromthento t (t + (t-f)) to
    | f>=t && t>=to = f: enumfromthento t (t - (f-t)) to
    | otherwise = [f]

--3
juntar_listas:: [a]->[a]->[a]
juntar_listas [] l2 = l2
juntar_listas (x:xs) l2 = x:(juntar_listas xs l2)

--4
(!!):: [a]->Int->a
(!!) (x:xs) i 
    | i == 0 = x
    | otherwise = xs Main.!! (i-1)

--5
reverse':: [a]->[a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

r1:: [a]->[a]
r1 [] = []
r1 l = racc [] l
    where
        racc acc [] = acc
        racc acc (h:t) = racc (h:acc) t 

--6
take:: Int->[a]->[a]
take _ [] = []
take 0 _ = []
take n (x:xs) = x: Main.take (n-1) xs

--7
drop:: Int->[a]->[a]
drop _ [] = []
drop 0 l = l
drop n (_:xs) = Main.drop (n-1) xs

--8
zip':: [a]->[b]->[(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (a:as) (b:bs) = (a,b): zip' as bs

--9
elem':: Eq a => a->[a]->Bool
elem' _ [] = False
elem' n (h:hs) 
    | n == h = True
    | otherwise = elem' n hs

--10
replicate':: Int->a->[a]
replicate' 0 _ = []
replicate' n x = x: replicate' (n-1) x

--11
intersperse':: a->[a]->[a]
intersperse' _ [] = []
intersperse' _ (h:[]) = [h]
intersperse' n (h:hs) = h:n:intersperse' n hs

--12
{-
group':: Eq a => [a]->[[a]]
group' [] = [[]]
group' (h:hs) = takeWhile (==h) (h:hs) : group' (dropWhile (==h) (h:hs))
-}

agrupa:: Eq a => [a]->[[a]]
agrupa [] = []
agrupa (h:t) = loopa [h] t
    where
        loopa hList [] = [hList]
        loopa hList (x:xs)
            | head hList == x = loopa (hList ++ [x]) xs
            | otherwise = hList: loopa[x] xs

--13
concat'::Eq a => [[a]]->[a]
concat' [] = []
concat' (h:hs) = h ++ concat' hs
--concat' ((h:hs):hss) = h:concat'(hs:hss)
--concat' ([]:hss) = concat' hss

--14
inits':: [a]->[[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

--15
tails':: [a]->[[a]]
tails' [] = [[]]
tails' (h:t) = (h:t) : tails' t
--tails' l =  [l] ++ tails' (tail l)

--16
isPrefixOf':: Eq a=> [a]->[a]->Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' l1 l2
    | l1 == l2 = True
    | otherwise = isPrefixOf' l1 (init l2)

--17
isSuffixOf':: Eq a=> [a]->[a]->Bool
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' l1 (h:t)
    | l1 == (h:t) = True
    | otherwise = isSuffixOf' l1 t

--18
isSubsequenceOf':: Eq a => [a]->[a]->Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (h1:t1) (h2:t2)
    | h1 == h2 = isSubsequenceOf' t1 t2
    | otherwise = isSubsequenceOf' (h1:t1) t2

--19

elemIndices':: Eq a => a->[a]->[Int]
elemIndices' _ [] = []
elemIndices' x l = indicesAux x l 0
    where
        indicesAux _ [] _ = []
        indicesAux x (h:t) n
            | x==h = n:indicesAux x t (n+1)
            | otherwise = indicesAux x t (n+1)

--holy isto funciona
--elemIndices' n l = [i | (y,i)<-zip l [0..] ,y==n]
                    --i é o que retorna, o zip faz um par onde o primeiro é um elemento de l (igual ao n) e vai metendo pares (y,0) (y,1)...
                    --sempre que é igual mas só retorna o número (índice do elemento)

--20
nub':: Eq a => [a]->[a]
nub' [] = []
nub' (h:t) = h: nub' (nubaux h t)

nubaux:: Eq a => a -> [a] -> [a]
nubaux x (h:t) 
    | x == h = nubaux x t
    | otherwise = h:nubaux x t
nubaux _ _ = []

--21
delete':: Eq a => a->[a]->[a]
delete' x (h:t)
    | x==h = t
    | otherwise = h: delete' x t
delete' _ _ = []

--22
vinteedois:: Eq a => [a] -> [a] -> [a]
vinteedois l (h:t) = vinteedois (delete' h l) t
vinteedois l _ = l

--23
union':: Eq a => [a]->[a]->[a]
union' l1 l2 = l1 ++ (vinteedois l2 l1)

--24
intersect':: Eq a => [a]->[a]->[a]
intersect' _ [] = []
intersect' [] _ = []
intersect' l (h:t) = interaux l h ++ intersect' l t

interaux:: Eq a => [a]->a->[a]
interaux (h:t) x 
    | h == x = h: interaux t x
    | otherwise = interaux t x
interaux _ _ = []

--25
insert':: Ord a => a->[a]->[a]
insert' n [] = [n]
insert' n (h:t)
    | n <= h = n : (h:t)
    | otherwise = h: insert' n t

--26
unwords':: [String]->String
unwords' [] = ""
unwords' (h:t) = h ++ " " ++ unwords' t

--27
unlines':: [String]->String
unlines' [] = ""
unlines' (h:t) = h ++ "\n" ++ unlines' t

--28
{-
pMaior':: Ord a => [a]->Int
pMaior' (h:t) 
    | h == aux (h:t) = 0
    |   otherwise = 1 + pMaior' t
    where
        aux [x] = x
        aux (h1:h2:t)
            | h1 >= h2 = aux (h1:t)
            | otherwise = aux (h2:t)
-}
pMaior':: Ord a => [a]->Int
pMaior' l = snd (pMaiorAux l)

pMaiorAux:: Ord a => [a]->(a,Int)
pMaiorAux (h:t)
    | h > m = (h,0)
    | otherwise = (m, p+1)
        where (m,p) = pMaiorAux t

{-
pMaior' (_:[]) = 0
pMaior' (h:t) 
    | h == pMaiorAux (h:t) = 0
    | otherwise = 1 + pMaior' t

pMaiorAux:: Ord a => [a]->a
pMaiorAux (h:[]) = h
pMaiorAux (h1:h2:t) 
    | h1 >= h2 = pMaiorAux (h1:t)
    | h1 < h2 = pMaiorAux (h2:t)
-}

--30
algarismos:: [Char]->[Char]
algarismos (h:t)
    | h <= '9' && h >= '0' = h: algarismos t
    | otherwise = algarismos t

--31
posImpares:: [a]->[a]
posImpares l = aux l 0
    where
        aux [] _ = []
        aux (h:t) n 
            | mod n 2 == 1 = h: aux t (n+1)
            | otherwise = aux t (n+1)
{-
aux:: [a]->Int->[a]
aux [] _ = []
aux (h:t) n 
    | mod n 2 == 1 = h: aux t (n+1)
    | otherwise = aux t (n+1)
-}

--32
posPares:: [a]->[a]
posPares l = aux l 0
    where
        aux [] _ = []
        aux (h:t) n 
            | mod n 2 == 0 = h: aux t (n+1)
            | otherwise = aux t (n+1)
{-
aux:: [a]->Int->[a]
aux [] _ = []
aux (h:t) n 
    | mod n 2 == 0 = h: aux t (n+1)
    | otherwise = aux t (n+1)
-}

--33
isSorted:: Ord a => [a]->Bool
isSorted [] = True
isSorted (_:[]) = True
isSorted (h:t)
    | h <= (head t) = isSorted t
    | otherwise = False

--34
{-
iSort:: Ord a => [a]->[a]
iSort [] = []
iSort [h] = [h]
iSort (h:t) = insert' h (iSort t)

insert':: Ord a => a->[a]->[a]
insert' n [] = [n]
insert' n (h:t)
    | n <= h = n : (h:t)
    | otherwise = h: insert' n t
-}

--melhor delas todas por alguma razão...
mergeSort:: Ord a => [a]->[a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort l = merge (mergeSort l1) (mergeSort l2)
    where
        --l1 = mergeSort (Main.take (div (length l) 2) l)
        --l2 = mergeSort (Main.drop (div (length l) 2) l)
        (l1,l2) = parte l
        merge:: Ord a => [a]->[a]->[a]
        merge [] b = b
        merge a [] = a
        merge (a:as) (b:bs)
            | a<b = a: merge as (b:bs)
            | otherwise = b: merge (a:as) bs
        parte:: [a]->([a],[a])
        parte [] = ([],[])
        parte [x] = ([x],[])
        parte (x1:x2:xs) = (x1:xs1, x2:xs2)
            where
                (xs1,xs2) = parte xs
{-
iSort [] =[]
iSort (h:t) = (iSort (men h t)) ++ h:(iSort (mai h t))
    where
        men _ [] = []
        men h (x:xs) | h<=x = x: men h xs
                     | otherwise = men h xs
        mai _ [] = []
        mai h (x:xs) | h>x = x: mai h xs
                     | otherwise = mai h xs

    MELHORANDO...

iSort [] =[]
iSort (h:t) = (iSort (men h t)) ++ h:(iSort (mai h t))
    where
        men = [x | x <- t, x<=h]
        mai = [x | x <- t, x>h]

    MELHORANDO...

iSort [] = []
iSort (h:t) = (iSort men) ++ (h: (iSort mai))
    where
        (men,mai) = menMai h t --par de listas, primeira dos maiores e segunda dos menores     
        menMai::Ord a=> a->[a]->([a],[a])
        menMai _ [] = ([],[])
        menMai x (h:t)  | h<=x = ((h:a),b) --mete dentro dos menores
                        | otherwise = (a,(h:b)) --mete dentro dos maiores
                            where
                            (a,b) = menMai x t --par de listas que eventualmente serão vazias por recursividade
-}
{-
Outra forma...

iSort [] = []
iSort l = h: (iSort t)
    where 
        h = minimum' l --elemento mínimo da lista l
        t = delete' h l --tirar o elemento mínimo da lista l

delete':: Eq a => a->[a]->[a]
delete' x (h:t)
    | x==h = t
    | otherwise = h: delete' x t
delete' _ _ = []

minimum':: Ord a => [a]->a
minimum' [] = error "Empty List"
minimum' [x] = x
minimum' (h1:h2:t) 
    | h1<h2 = minimum'(h1:t)
    | h2<h1 = minimum'(h2:t)
    | otherwise = minimum'(h1:t)
--minimum' (h:t) = min h (minimum' t)
-}

--35
menor:: String->String->Bool
menor _ [] = False
menor [] _ = True
menor (h1:t1) (h2:t2)
    | h1==h2 = menor t1 t2
    | h1<h2 = True
    | otherwise = False               

--36
elemMSet:: Eq a => a->[(a,Int)]->Bool
elemMSet _ [] = False
elemMSet n ((h,_):t)
    | n == h = True
    | otherwise = elemMSet n t

--37
lengthMSet:: [(a,Int)]->Int
lengthMSet [] = 0
lengthMSet ((_,n):t) = n + length t

--38
converteMSet:: [(a,Int)]->[a]
converteMSet [] = []
converteMSet ((h,1):t) = h:converteMSet t
converteMSet ((h,n):t) = h:converteMSet ((h,n-1) :t)

--39
insereMSet:: Eq a => a->[(a,Int)]->[(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((b,n):t)
    | a == b = ((a,n+1):t)
    | otherwise = (b,n):insereMSet a t

--40
removeMSet:: Eq a => a->[(a,Int)]->[(a,Int)]
removeMSet _ [] = []
removeMSet a ((b,n):t)
    | a==b && n==1 = t
    | a==b = (b,n-1):t
    | otherwise = (b,n):removeMSet a t

--41
constroiMSet1:: Ord a => [a]-> [(a,Int)]
constroiMSet1 [] = []
constroiMSet1 (h:t) = insereMSet h (constroiMSet1 t)

{-
insereMSet:: Eq a => a->[(a,Int)]->[(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((b,n):t)
    | a == b = ((a,n+1):t)
    | otherwise = (b,n):insereMSet a t
-}

--42
partitionEithers:: [Either a b]->([a],[b])
partitionEithers [] = ([],[])
partitionEithers (h:t) = case h of
    Left a -> ((a:x),y)
    Right b -> (x,(b:y))
    where
        (x,y) = (partitionEithers t)
    
--43
catMaybes:: [Maybe a]->[a]
catMaybes (x:xs) = case x of
    Nothing -> catMaybes xs
    Just a -> [a] ++ catMaybes xs

data Movimento = Norte | Sul | Este | Oeste deriving Show

--44
posicao:: (Int,Int)->[Movimento]->(Int,Int)
posicao a [] = a
posicao (a,b) (x:xs) = case x of
    Norte -> posicao(a,b+1) xs
    Sul -> posicao(a,b-1) xs
    Este -> posicao (a+1,b) xs
    Oeste -> posicao (a-1,b) xs

--45
caminho:: (Int,Int)->(Int,Int)->[Movimento]
caminho (xi,yi) (xf,yf)
    | xi == xf && yi == yf = []
    | xi>xf = (Oeste: caminho (xi-1,yi) (xf,yf))
    | xi<xf = (Este: caminho (xi+1,yi)(xf,yf))
    | yi>yf = (Sul: caminho (xi,yi-1)(xf,yf))
    | otherwise = (Norte: caminho(xi,yi+1)(xf,yf))

--46
vertical:: [Movimento]->Bool
vertical [] = True
vertical (e:es)=case e of
    Este->False
    Oeste->False
    _->vertical es

data Posicao = Pos Int Int  deriving Show
--47
maisCentral::[Posicao]->Posicao
maisCentral [a] = a
maisCentral ((Pos x1 y1):(Pos x2 y2):t)
    | raio1 <= raio2 = maisCentral ((Pos x1 y1):t)
    | otherwise = maisCentral ((Pos x2 y2):t)
        where
            raio1 = sqrt(fromIntegral ((x1^2) + (y1^2)))
            raio2 = sqrt(fromIntegral ((x2^2) + (y2^2)))

--48
vizinhos:: Posicao->[Posicao]->[Posicao]
vizinhos _ [] = []
vizinhos (Pos x1 y1) ((Pos x2 y2):t)
    | x2 == x1-1 || x2 == x1+1 || y2 == y1-1 || y2 == y1+1 = (Pos x2 y2): vizinhos (Pos x1 y1) t
    | otherwise = vizinhos (Pos x1 y1) t

--49
mesmaOrdenada:: [Posicao]->Bool
mesmaOrdenada [] = True
mesmaOrdenada ((Pos _ y1):(Pos _ y2):t)
    | y1 /= y2 = False
    | otherwise = mesmaOrdenada t

data Semaforo = Verde | Amarelo | Vermelho deriving Show
--50
interseccaoOK:: [Semaforo]->Bool
interseccaoOK l = aux l 0
    where
        aux _ 2 = False
        aux [] _ = True
        aux (e:es) n = case e of
            Verde->aux es (n+1)
            _->aux es n
