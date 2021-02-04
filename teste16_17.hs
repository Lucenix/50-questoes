
--1
type MSet a = [(a,Int)]

--a
cardMSet:: MSet a -> Int
cardMSet [] = 0
cardMSet ((_,d):t) = d + cardMSet t
--b
moda:: MSet a -> [a]
moda ((a,d):t) = aux (a,d) t
    where
        aux:: (a,Int)->MSet a->[a]
        aux (a,d) [] = s a d
        aux (a,d) ((b,c):t)
            | c > d = aux (b,c) t
            | otherwise = aux (a,d) t

s:: a->Int->[a]
s _ 0 = []
s a x = a:(s a (x-1))

--2
data SReais = AA Double Double | FF Double Double
    | AF Double Double | FA Double Double
    | Uniao SReais SReais
--a
instance Show SReais where
    show (AA a b) = "]" ++ show a ++ "," ++ show b ++ "["
    show (FF a b) = "[" ++ show a ++ "," ++ show b ++ "]"
    show (AF a b) = "]" ++ show a ++ "," ++ show b ++ "]"
    show (FA a b) = "[" ++ show a ++ "," ++ show b ++ "["
    show (Uniao e d) = show e ++ " U " ++ show d
--b
pertence:: Double->SReais->Bool
pertence x (Uniao e d) = pertence x e || pertence x d
pertence x (AA a b) = x > a && x < b
pertence x (FF a b) = x >= a && x <= b
pertence x (AF a b) = x > a && x <= b
pertence x (FA a b) = x >= a && x < b

--c
tira:: Double->SReais->SReais
tira x (Uniao e d) 
    | pertence x e = tira x e
    | pertence x d = tira x d
    | otherwise = (Uniao e d)
tira x (AA a b)
    | pertence x (AA a b) = (Uniao (AA a x) (AA x b))
    | otherwise = (AA a b)
tira x (FF a b)
    | pertence x (FF a b) = (Uniao (FA a x) (AF x b))
    | otherwise = (FF a b)
tira x (AF a b)
    | pertence x (AF a b) = (Uniao (AA a x) (AF x b))
    | otherwise = (AF a b)
tira x (FA a b)
    | pertence x (FA a b) = (Uniao (FA a x) (AA x b))
    | otherwise = (FA a b)

--3
data RTree a = R a [RTree a]

tree1 :: RTree Integer
tree1 = R 1 [R 2 [],
             R 3 [R 4 [R 5 [],
                       R 6 []
                      ]
                 ],
             R 7 []
            ]
--a
percorre:: [Int]->RTree a->Maybe [a]
percorre (1:xs) (R x (h:t)) = case y of
    Nothing -> Nothing 
    Just a -> Just (x:a)
    where
        y = percorre xs h
percorre [] (R x []) = Just [x]
percorre _ (R x []) = Nothing  
percorre (a:b) (R x (h:t)) = percorre (a-1:b) (R x t)    
--b
procura:: Eq a => a -> RTree a -> Maybe [Int]
procura x (R y []) 
    | x == y = Just []
    | otherwise = Nothing 
procura x (R y (h:t))
    | x == y = Just []
    | otherwise = case a of
        Nothing -> case z of
            Nothing -> Nothing
            Just (h:t) -> Just (h+1:t)
        Just b -> Just (1:b)
        where
            a = procura x h
            z = procura x (R y t)