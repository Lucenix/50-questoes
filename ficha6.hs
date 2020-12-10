

data BTree a = Empty
             | Node a (BTree a) (BTree a)
            deriving Show

--1
--1a
altura:: BTree a -> Int
altura Empty = 0
altura (Node _ e d) = 1 + max (altura e) (altura d)

--1b
contaNodos:: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node _ e d) = 1 + contaNodos e + contaNodos d

--1c
folhas:: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ e d) = folhas e + folhas d

--1d
prune:: Int->BTree a -> BTree a
prune 1 _ = Empty
prune _ Empty = Empty
prune n (Node a e d) = Node a (prune (n-1) e) (prune (n-1) d)

--1e
path:: [Bool]->BTree a->[a]
path [] _ = []
path _ Empty = []
path (False:bs) (Node a e _) = a:path bs e
path (True:bs) (Node a _ d) = a:path bs d

--1f
mirror:: BTree a -> BTree a
mirror Empty = Empty
mirror (Node a e d) = Node a (mirror d) (mirror e)

--1g
zipWithBT:: (a->b->c)->BTree a ->BTree b ->BTree c
zipWithBT _ Empty _ = Empty
zipWithBT _ _ Empty = Empty
zipWithBT f (Node a e1 d1) (Node b e2 d2) = Node (f a b) (zipWithBT f e1 e2) (zipWithBT f d1 d2)

--1h
unzipBT:: BTree (a,b,c)->(BTree a,BTree b, BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) e d) = (Node a a1 a2, Node b b1 b2, Node c c1 c2)
    where
        (a1, b1, c1) = unzipBT e
        (a2, b2, c2) = unzipBT d

--aula do dia 3/12 (Árvores Binárias de Procura e eficiência)
acrescenta:: Ord a => a-> BTree a -> BTree a
acrescenta a Empty = Node a Empty Empty
acrescenta x (Node a e d) 
    | x < a = Node a (acrescenta x e) d
    | otherwise = Node a e (acrescenta x d)

maiorProc:: Ord a => BTree a -> a
maiorProc (Node x _ Empty) = x
maiorProc (Node _ _ d) = maiorProc d