import Data.List

--1

--1a
any':: (a->Bool)->[a]->Bool
any' _ [] = False
any' f (h:t)
    | f h = True
    | otherwise = any' f t

--1b
zipWith':: (a->b->c)->[a]->[b]->[c]
zipWith' _ [] [] = []
zipWith' f (x:xs) (y:ys) = f x y:zipWith' f xs ys

--1c
takeWhile':: (a->Bool)->[a]->[a]
takeWhile' _ [] = []
takeWhile' f (h:t)
    | f h = h:takeWhile' f t
    | otherwise = []

--1d
dropWhile':: (a->Bool)->[a]->[a]
dropWhile' _ [] = []
dropWhile' f (h:t)
    | f h = dropWhile' f t
    | otherwise = h:t

--1e
span'::(a->Bool)->[a]->([a],[a])
span' _ [] = ([],[])
span' f (h:t)
    | f h = (h:x,y)
    | otherwise = (x,t)
        where
            (x,y) = span' f t

--1f
deleteBy'::(a->a->Bool)->a->[a]->[a]
deleteBy' _ _ [] = []
deleteBy' f x (y:ys)
    | f x y = ys
    | otherwise = y:deleteBy' f x ys

--1g
sortOn'::Ord b =>(a->b)->[a]->[a]
sortOn' _ [] = []
sortOn' f (h:t) = inserta f h (sortOn' f t)
    where
        inserta _ h [] = [h]
        inserta f x (h:t)
            | f x <= f h = x:h:t
            | otherwise = h:inserta f x t

--2
type Polinomio = [Monomio]
type Monomio = (Float,Int)

--2a
selgrau::Int->Polinomio->Polinomio
selgrau _ [] = []
selgrau g p = filter (\x -> g == snd x) p

--2b
conta::Int->Polinomio->Int
conta n = foldl (\acc (_,g) -> if (n == g) then acc+1 else acc) 0

--2c
grau::Polinomio->Int
grau ((_,x):t) = ordem (\max (_,g) -> if (max >= g) then max else g) x t
    where
        ordem _ max [] = max
        ordem f max (h:t) = ordem f (f max h) t

--2d
deriv::Polinomio->Polinomio
deriv = map (\(x,y) -> (x*fromIntegral y,y-1)) 

--2e
calcula:: Float->Polinomio->Float
calcula x = foldl (\acc (c,g) -> acc + c*x^g) 0 

--2f
simp::Polinomio->Polinomio
simp = filter (\(_,g)-> g/=0)

--2g
mult::Monomio->Polinomio->Polinomio
mult (c,g) = map (\(x,y) -> (x*c,g+y))

--2h
ordena::Polinomio->Polinomio
ordena = sortOn' snd

--2i (está a por o elemento com o maior grau no início: o foldl 'acaba')
normaliza:: Polinomio->Polinomio
normaliza p = ordena (normaliza' (ordena p))
    where
    normaliza' [] = []
    normaliza' (m:p) = foldl (\((c,e):ms) (c1,e1)-> if (e == e1) then (c+c1,e1):ms else (c1,e1):(c,e):ms) [m] p

--2j
soma::Polinomio->Polinomio->Polinomio
soma p1 p2 = normaliza (p1++p2)

--2k
produto:: Polinomio->Polinomio->Polinomio
produto p1 p2 = concatMap (\m -> mult m p2) p1

--2l
equiv:: Polinomio->Polinomio->Bool
equiv p1 p2 = equiv' (normaliza p1) (normaliza p2)
    where
        equiv' p1 p2 = and (zipWith (==) p1 p2)

--3
type Mat a = [[a]]
--3a
dimOK :: Mat a -> Bool
dimOK [] = True
dimOK (l:ls) = aux (length l) ls
    where
        aux _ [] = True
        aux x (l:ls)
            | x /= length (l:ls) = False
            | otherwise = aux x ls
--dimOK [] = False
--dimOK (l:ls) = all (\l' -> length l' == length l) ls

--3b
dimMat:: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat (l:ls) = (length (l:ls), length l)

--3c
addMat:: Num a => Mat a -> Mat a -> Mat a
addMat = zipWith (zipWith (+))

--3d
transpose':: Mat a -> Mat a
transpose' m1 = map head m1 : transpose' (map tail m1)

--3e
multMat:: Num a => Mat a -> Mat a -> Mat a
multMat m1 m2 = if (snd (dimMat m1) == fst (dimMat m2)) then ([[sum (zipWith (*) l1 l2) | l2 <- transpose' m2] | l1 <- m1]) else (error "Not possible")

--3f
zipWMat:: (a->b->c)->Mat a -> Mat b -> Mat c
zipWMat f ([e1]:l1:lss) ([e2]:l2:lzz) = [(f e1 e2)]:((zipWith f l1 l2):(zipWMat f lss lzz))

--3g
triSup:: (Num a, Eq a) => Mat a -> Bool
triSup [] = True
triSup (_:ms) = aux 1 ms
    where
        aux _ [] = True
        aux n (l:ls) = auxx n l && aux (n+1) ls
            where
                auxx 0 _ = True
                auxx n (e:es)
                    | e == 0 = auxx (n-1) es
                    | otherwise = False

--Idea erradas (o produto ser 0 não me garante que o primeiro elemento é zero):
--triSup:: Num a => Mat a -> Bool
--triSup [] = True
--triSup (_:ms) = all f ms
--    where
--        f ms = product ms == 0

--3h
rotateLeft:: Mat a -> Mat a
rotateLeft m = map last m : rotateLeft (map init m)