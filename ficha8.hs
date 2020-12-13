--1
data Frac = F Integer Integer deriving Show
--1a
normaliza:: Frac->Frac
normaliza (F n d) = F (div n x) (div d x)
    where
        x = mdc n d

mdc:: Integer->Integer->Integer
mdc x y
    | x == y = x
    | x>y = mdc (x-y) y
    | otherwise = mdc x (y-x)

--1b
instance Eq Frac where
    f1 == f2 = aux (normaliza f1) (normaliza f2)
        where
            aux (F n1 d1) (F n2 d2) = n1==n2 && d1==d2

--1c
instance Ord Frac where
    (F n1 d1) <= (F n2 d2) = (fromIntegral n1 / fromIntegral d1) <= (fromIntegral n2 / fromIntegral d2)
