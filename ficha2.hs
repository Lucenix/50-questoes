--ficha 2

--2a
dobros:: [Float]->[Float]
dobros [] = []
dobros (h:t) = 2*h : dobros t

--2b
numOcorre:: Char->String->Int
numOcorre _ [] = 0
numOcorre c (h:t) 
    | c == h = 1 + numOcorre c t
    | otherwise = 0 + numOcorre c t

--2c
positivos:: [Int]->Bool
positivos [] = False
positivos (h:t) 
    | h >= 0 = False
    | h >= 0 && t == [] = True
    | otherwise = positivos t

--2d
soPos:: [Int]->[Int]
soPos [] = []
soPos (h:t)
    | h >= 0 = soPos t
    | otherwise = h : soPos (t)

--2e
somaNeg:: [Int]->Int
somaNeg [] = 0
somaNeg (h:t)
    | h<0 = h +  somaNeg t
    | otherwise = somaNeg t

--2f
tresUlt:: [a]->[a]
tresUlt (h:t)
    | length (h:t) <= 3 = h:t
    | otherwise = tresUlt t

--2g
segundos:: [(a,b)]->[b]
segundos [] = []
segundos ((_,b):t) = b:segundos t

--2h
nosPrimeiros:: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros a ((h,_):t)
    | a == h = True
    | otherwise = nosPrimeiros a t

--2i
sumTriplos:: (Num a, Num b, Num c) => [(a,b,c)]->(a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):(a1,b1,c1):t) = sumTriplos ((a+a1,b+b1,c+c1):t)
sumTriplos [a] = a

type Polinomio = [Monomio]
type Monomio = (Float, Int)

--4b
grau::Polinomio->Int
grau [(_,e)] = e
grau ((c1,e1):(c2,e2):t)
    | e1>= e2 = grau((c1,e1):t)
    | otherwise = grau((c2,e2):t)

--4c
selgrau::Int->Polinomio->Polinomio
selgrau _ [] = []
selgrau n ((c,e):t)
    | n == e = (c,e) : selgrau n t
    | otherwise = selgrau n t

--4h
normaliza:: Polinomio->Polinomio
normaliza [] =[]
normaliza p = auxNorm p (grau p)

auxNorm:: Polinomio->Int->Polinomio
auxNorm [] _ = [] 
auxNorm _ (-1) = []
auxNorm p n = (auxx (selgrau n p)) ++ (auxNorm p (n-1))
    where
        auxx [] = []
        auxx (a:[]) = [a]
        auxx ((c1,e1):(c2,_):t) = auxx ((c1+c2,e1):t)

--4i
soma::Polinomio->Polinomio->Polinomio
soma p1 p2 = normaliza (p1++p2)

--4k
ordena:: Polinomio->Polinomio
ordena [] = []
ordena ((c,e):t) = (auxOrd (c,e) (ordena t))

auxOrd::Monomio->Polinomio->Polinomio
auxOrd (c,e) [] = [(c,e)]
auxOrd (c1,e1) ((c2,e2):t)
    | e1 <= e2 = ((c1,e1): (c2,e2): t)
    | otherwise = (c2,e2): auxOrd (c1,e1) t

--4l
equiv:: Polinomio->Polinomio->Bool
equiv p1 p2 = aux (ordena (normaliza p1)) (ordena (normaliza p2))
    where
        aux [] [] = True
        aux [] _ = False
        aux _ [] = False
        aux ((c1,e1):t1) ((c2,e2):t2)
            | c1==c2 && e1==e2 = aux t1 t2
            |otherwise = False