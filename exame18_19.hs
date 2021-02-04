
--1
--a
isSorted:: (Ord a)=>[a]->Bool
isSorted [] = True 
isSorted (h:hs:t)
    | h < hs = isSorted (hs:t)
    | otherwise = False 

--b
inits:: [a]->[[a]]
inits l = aux [] l
    where
        aux acc [] = [acc]
        aux acc (h:t) = acc:aux (acc++[h]) t

--2
maximumMB:: (Ord a) => [Maybe a] -> Maybe a
maximumMB l = aux (Nothing) l
    where
        aux:: (Ord a) => Maybe a -> [Maybe a] -> Maybe a
        aux acc [] = acc
        aux acc (Nothing:t) = aux acc t
        aux Nothing ((Just e):t) = aux (Just e) t
        aux (Just e) ((Just d):t)
            | d>e = aux (Just d) t
            | otherwise = aux (Just e) t

--3
data LTree a = Tip a | Fork (LTree a) (LTree a)
--a
listaLT:: LTree a -> [a]
listaLT (Fork e d) = listaLT e ++ listaLT d
listaLT (Tip a) = [a]
--b
instance (Show a) => Show (LTree a) where
    show (Fork e d) = "."++(show e) ++ "\n." ++ (show d)
    show (Tip a) = show a

--4
maxSumInit:: (Num a, Ord a)=> [a]->a
maxSumInit (h:t) = aux h t
    where
        aux x [] = x
        aux x (y:ys)
            | x < y+x = aux (y+x) ys
            | otherwise = aux x ys

--5
type RelP a = [(a,a)]
type RelL a = [(a,[a])]
type RelF a = ([a], a->[a])
--a

convLP :: RelL a -> RelP a
convLP l = concat (map junta l)
    where junta (x,xs) = map (\y->(x,y)) xs

convPL:: (Eq a) => RelP a-> RelL a
convPL ((a,b):t) = aux (a,[b]) t []
    where
        aux::(Eq a)=>(a,[a])->RelP a->RelP a->RelL a
        aux (a,l) [] [] = [(a,l)]
        aux (a,l) [] ((b,c):t) = (a,l):aux (b,[c]) t [] 
        aux (a,l) ((a1,b1):t) acc
            | a == a1 = aux (a,b1:l) t acc
            | otherwise = aux (a,l) t ((a1,b1):acc)

--b
criaRelPint:: Int->IO (RelP Int)
criaRelPint 0 = return []
criaRelPint x = do  putStrLn "Introduza uma abcissa."
                    x <- getLine
                    putStrLn "Introduza uma ordenada."
                    y <- getLine
                    ((read x,read y) :) <$> criaRelPint (n - 1)

n :: Int
n = error "not implemented"
                
--c
--i
convFP:: (Eq a)=> RelF a -> RelP a
convFP (l,f) = convLP (zip l (map f l))

--ii
convPF:: (Eq a)=> RelP a -> RelF a
convPF e = aux (convPL e)
    where
        aux l = (map fst l,f)
            where
                f a = foldl (\acc (b,c) -> if a == b then c else acc) [] l

--convPL:: (Eq a) => RelP a-> RelL a
--convPL l = aux [] (sortOn (fst) l)
--
--aux::(Eq a)=>[a]->RelP a->RelL a
--aux _ [(a,o)] = [(a,[o])]
--aux acc ((a,o):(a1,o1):t)
--    | a == a1 = aux (acc++[o]) ((a1,o1):t)
--    | otherwise = (a,acc++[o]):(aux [] ((a1,o1):t))
--
--sortOn:: (Ord b) => (a->b)->[a]->[a]
--sortOn f [] = []
--sortOn f (h:t) = inserta f h (sortOn f t)
--    where
--        inserta f h [] = [h]
--        inserta f h (x:xs)
--            | f h <= f x = h:x:t
--            | otherwise = x:inserta f h xs

--aux:: (Eq a) => RelP a -> (a,a) -> (a,[a]) 
--aux [] (a,o) = (a,[o])
--aux ((a1,o1):t) (a,o2)
--    | a1 == a = (y,o1:x)
--    | otherwise = (y,x)
--        where
--            (y,x) = aux t (a,o2)