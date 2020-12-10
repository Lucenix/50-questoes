
--4
--4a
--4b
data LTree a = Tip a | Fork (LTree a) (LTree a)
data BTree a = Empty | Node a (BTree a) (BTree a)
data FTree a b = Leaf b | No a (FTree a b) (FTree a b) deriving Show

joinFTrees:: BTree a -> LTree b -> Maybe (FTree a b)
joinFTrees Empty (Tip b) = Just (Leaf b)
joinFTrees (Node x e1 d1) (Fork e2 d2) = case joinFTrees e1 e2 of 
    Nothing -> Nothing
    Just f -> case joinFTrees d1 d2 of
        Just g -> Just (No x f g)
        Nothing -> Nothing
joinFTrees _ _ = Nothing
--Isto em baixo não funciona porque não considera se f g e são Nothing, assume sempre que são alguma coisa
--joinFTrees Empty (Tip b) = Just (Leaf b)
--joinFTrees (Node x e1 d1) (Fork e2 d2) = Just (No x f g)
--    where
--        Just f = joinFTrees e1 e2
--        Just g = joinFTrees d1 d2
--joinFTrees _ _ = Nothing