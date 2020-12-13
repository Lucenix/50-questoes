--1
data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

--1a
calcula:: ExpInt->Int
calcula exp = case exp of
    Const n -> n
    Simetrico e -> -(calcula e)
    Mais e1 e2 -> (+) (calcula e1) (calcula e2)
    Menos e1 e2 -> (-) (calcula e1) (calcula e2)
    Mult e1 e2 -> (*) (calcula e1) (calcula e2)

--1b
infixa:: ExpInt -> String
infixa exp = case exp of
    Const n -> show n
    Simetrico e -> " - " ++ (infixa e)
    Mais e1 e2 -> "( " ++ (infixa e1) ++ " + " ++ (infixa e2) ++ " )"
    Menos e1 e2 -> "( " ++ (infixa e1) ++ " - " ++ (infixa e2) ++ " )"
    Mult e1 e2 -> "( " ++ (infixa e1) ++ " x " ++ (infixa e2) ++ " )"

--1c
posfixa:: ExpInt -> String
posfixa exp = case exp of
    Const n -> show n
    Simetrico e -> (posfixa e) ++ " - " 
    Mais e1 e2 -> "( " ++ (posfixa e1) ++ (posfixa e2) ++ " + " ++  " )"
    Menos e1 e2 -> "( " ++ (posfixa e1) ++ (posfixa e2) ++ " - " ++  " )"
    Mult e1 e2 -> "( " ++ (posfixa e1) ++ (posfixa e2) ++ " x " ++  " )"

--2
data RTree a = R a [RTree a]

--2a
soma:: Num a => RTree a -> a
soma (R a l) = a + sum (map soma l)

--2b
altura:: RTree a -> Int
altura (R _ l) = 1 + maximum (map altura l)

--2c
prune:: Int -> RTree a -> RTree a
prune 1 (R a _) = R a []
prune n (R a l) = R a (map (prune (n-1)) l)

--2d
mirror:: RTree a -> RTree a
mirror (R a []) = R a []
mirror (R a l) = R a (reverse (map mirror l))

--2e
postorder:: RTree a -> [a]
postorder (R a []) = [a]
postorder (R a l) = concat (map postorder l) ++ [a]

--3
data LTree a = Tip a | Fork (LTree a) (LTree a)

--3a
ltSum:: Num a => LTree a -> a
ltSum (Tip a) = a
ltSum (Fork e d) = ltSum e + ltSum d

--3b
listaLT:: LTree a -> [a]
listaLT (Tip a) = a
listaLT (Fork e d) = listaLT e ++ listaLT d

--3c
ltHeight:: LTree a -> Int
ltHeight (Tip a) = 1
ltHeight (Fork e d) = max (ltHeight e) (ltHeight d)

--4
--data LTree a = Tip a | Fork (LTree a) (LTree a)
data BTree a = Empty | Node a (BTree a) (BTree a)
data FTree a b = Leaf b | No a (FTree a b) (FTree a b) deriving Show
--4a
splitFTree:: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf b) = (Empty, Tip b)
splitFTree (No a e d) = (Node a x y, Fork x' y')
    where
        (x,x') = splitFTree e
        (y,y') = splitFTree d

--4b
joinFTrees:: BTree a -> LTree b -> Maybe (FTree a b)
joinFTrees Empty (Tip b) = Just (Leaf b)
joinFTrees (Node x e1 d1) (Fork e2 d2) = case joinFTrees e1 e2 of 
    Nothing -> Nothing
    Just f -> case joinFTrees d1 d2 of
        Just g -> Just (No x f g)
        Nothing -> Nothing
joinFTrees _ _ = Nothing

--Isto em baixo não funciona porque não considera se (f g e) são Nothing, assume sempre que são alguma coisa
--joinFTrees Empty (Tip b) = Just (Leaf b)
--joinFTrees (Node x e1 d1) (Fork e2 d2) = Just (No x f g)
--    where
--        Just f = joinFTrees e1 e2
--        Just g = joinFTrees d1 d2
--joinFTrees _ _ = Nothing

