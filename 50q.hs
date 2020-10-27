
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
concat' [[]] = []
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
indicesAux:: Eq a=> a->[a]->Int->[Int]
indicesAux x (h:t) n
    | x == h = n: indicesAux x t (n+1)
    | otherwise = indicesAux x t (n+1)

elemIndices':: Eq a => a->[a]->[Int]
elemIndices' _ [] = []
elemIndices' x l = indicesAux x l 0

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
isSorted (_:[]) = True
isSorted (h:t)
    | h <= (head t) = isSorted t
    | otherwise = False

--34
iSort:: Ord a => [a]->[a]
iSort [] = []
iSort [h] = [h]
iSort (h:t) = insert' h (iSort t)

--35
