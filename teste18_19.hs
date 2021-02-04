
--1
--a
elemIndices:: Eq a => a -> [a] -> [Int]
elemIndices n (h:t) = aux n 0 (h:t)
    where
        aux _ _ [] = []
        aux x n (h:t)
            | x == h = n:aux x (n+1) t
            | otherwise = aux x (n+1) t
--b
isSubsequenceOf:: Eq a => [a]->[a]->Bool 
isSubsequenceOf [] _ = False 
isSubsequenceOf _ [] = True 
isSubsequenceOf (x:xs) (y:ys)
    | x == y = isSubsequenceOf xs ys
    | otherwise = isSubsequenceOf xs (y:ys)

--2
data BTree a = Empty | Node a (BTree a) (BTree a)
--a
lookupAP:: Ord a => a -> BTree (a,b) -> Maybe b
lookupAP _ Empty = Nothing 
lookupAP x (Node (a,b) e d)
    | x == a = Just b
    | x < a = lookupAP x e
    | otherwise = lookupAP x d

--b
zipWithBT:: (a->b->c)->BTree a->BTree b->BTree c
zipWithBT _ Empty _ = Empty
zipWithBT _ _ Empty = Empty
zipWithBT f (Node x e d) (Node y g h) = Node (f x y) (zipWithBT f e g) (zipWithBT f d h)

--4
data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)
--a
firstSeq:: Seq a -> a
firstSeq (Cons x _) = x
firstSeq (App Nil e) = firstSeq e
firstSeq (App e _) = firstSeq e
--b
dropSeq:: Int -> Seq a -> Seq a
dropSeq _ Nil = Nil
dropSeq 0 e = e
dropSeq n (Cons x e) = dropSeq (n-1) e
dropSeq n (App Nil d) = dropSeq n d
dropSeq n (App e Nil) = dropSeq n e
dropSeq n (App e d) 
    | n < x = App (dropSeq n e) d
    | n == x = d
    | otherwise = dropSeq (n-x) d
        where
            x = conta e

conta:: Seq a -> Int
conta Nil = 0
conta (Cons x e) = 1 + conta e
conta (App e d) = conta e + conta d
--c
instance Show a => Show (Seq a) where
    show s = "<<" ++ aux s ++ ">>"
        where
            aux Nil = ""
            aux (Cons a Nil) = show a
            aux (Cons a s) = show a ++ "," ++ aux s
            aux (App e d) = aux e ++ "," ++ aux d
            --aux s = show (firstSeq s) ++","++ show (firstSeq (dropSeq 1 s))

--5
type Mat a = [[a]]
----a
--getElem:: Mat a -> IO a
--getElem m = do  x <- randomRIO(0,length m-1)
--                let (a,c:b) = splitAt x m
--                y <- randomRIO (0,length c)
--                let (a1,c1:b1) = splitAt y c
--                return c1
--b
magic:: Mat Int -> Bool
magic m = linhas n m && colunas n m && diagonais n m
    where
        n = sum (head m)

linhas:: Int->Mat Int->Bool
linhas n m = all (==n) (map sum m)
colunas:: Int->Mat Int->Bool
colunas n m = all (==n) (map sum (transposta m))
    where
        transposta:: Mat Int -> Mat Int
        transposta [] = []
        transposta ((h:[]):hs) = [map head ((h:[]):hs)]
        transposta n = (map head n):(transposta (map tail n))
diagonais::Int->Mat Int->Bool 
diagonais n m = sum d1 == n && sum d2 == n
    where
        d1 = diag1 m
        d2 = diag2 m
diag1 :: [[a]] -> [a]
diag1 [h] = [head h]
diag1 (h:t) = head h:(diag1 (map tail t))
diag2 :: [[a]] -> [a]
diag2 [h] = [last h]
diag2 (h:t) = last h:(diag2 (map init t))