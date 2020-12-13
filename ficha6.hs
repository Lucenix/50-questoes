

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

--2
--2a
minimo:: Ord a => BTree a -> a
minimo (Node x Empty _) = x
minimo (Node _ e _) = minimo e

--2b
semMinimo:: Ord a => BTree a -> BTree a
semMinimo (Node _ Empty _) = Empty
semMinimo (Node x e d) = Node x (semMinimo e) d

--2c
minSmin:: Ord a => BTree a -> (a,BTree a)
minSmin (Node x Empty b) = (x,b)
minSmin (Node x e d) = (a,Node x b d)
    where
        (a,b) = minSmin e

--2d
remove:: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove y (Node x e d)
    | x == y = case d of
        Empty -> e
        _ -> Node m e d'
    | y < x = Node x (remove y e) d
    | otherwise = Node x e (remove y d)
        where
            (m,d') = minSmin d

--3
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                    | Rep
                    | Faltou
                    deriving Show
type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)

--3a
inscNum:: Numero->Turma->Bool
inscNum _ Empty = False
inscNum n (Node (num,_,_,_) e d)
    | n == num = True
    | n < num = inscNum n e
    | otherwise = inscNum n d

--3b
inscNome:: Nome->Turma->Bool
inscNome _ Empty = False
inscNome n (Node (_,nom,_,_) e d)
    | n == nom = True
    | otherwise = inscNome n e && inscNome n d

--3c
trabEst:: Turma->[(Numero,Nome)]
trabEst Empty = []
trabEst (Node (n,nome,TE,_) e d) = trabEst e ++ [(n,nome)] ++ trabEst d
trabEst (Node _  e d) = trabEst e ++ trabEst d

--3d
nota:: Numero->Turma->Maybe Classificacao
nota _ Empty = Nothing
nota n (Node (num,_,_,c) e d)
    | n == num = Just c
    | n < num = nota n e
    | otherwise = nota n d

--3e
percFaltas:: Turma->Float
percFaltas Empty = 0
percFaltas t = (n/d)*100
    where
        (n,d) = aux t
            where
                aux Empty = (0,0)
                aux (Node (_,_,_,c) e d) = case c of
                    Faltou -> (1+x+x',1+y+y')
                    _ -> (x+x',1+y+y')
                    where
                        (x,y) = aux e
                        (x',y') = aux d 

--3f
mediaAprov:: Turma->Float
mediaAprov Empty = 0
mediaAprov t = (n/d)
    where
        (n,d) = aux t
            where
                aux:: Turma->(Float,Float)
                aux Empty = (0,0)
                aux (Node (_,_,_,c) e d) = case c of
                    Aprov n -> (fromIntegral n+x+x1,y+y1+1)
                    _ -> (x+x1,y+y1+1)
                    where
                        (x,y) = aux e
                        (x1,y1) = aux d

--3g
aprovAv:: Turma->Float
aprovAv Empty = 0
aprovAv t = (n/d)
    where
        (n,d) = aux t
            where
                aux Empty = (0,0)
                aux (Node (_,_,_,c) e d) = case c of
                    Aprov n -> if (n>=10) then (1+x+x',1+y+y') else (x+x',1+y+y')
                    _ ->  (x+x',y+y')
                    where
                        (x,y) = aux e
                        (x',y') = aux d