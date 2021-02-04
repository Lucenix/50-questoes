--1
data Frac = F Integer Integer
--1a
normaliza:: Frac->Frac
normaliza (F n d) 
    | n<0 && d<0 = F (-(div n x)) (-(div d x))
    | n>0 && d<0 = F (-(div n x)) (div d x)
    | otherwise = F (div n x) (div d x)
    where
        x = mdc n d

mdc:: Integer->Integer->Integer
mdc x y = aux (abs x) (abs y)
    where
        aux x y
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

--1d
instance Show Frac where
    show (F n d) = (show n) ++ "/" ++ (show d)

--1e
instance Num Frac where
    (F n1 d1) + (F n2 d2) = F (n1*d2+n2*d1) (d1*d2)
    (F n1 d1) * (F n2 d2) = F (n1*n2) (d1*d2)
    (F n1 d1) - (F n2 d2) = F (n1*d2-n2*d1) (d1*d2)
    negate (F n d) = F (-n) d
    abs (F n d) = F (abs n) (abs d)
    signum (F n d)
        | n*d < 0 = -1
        | n*d > 0 = 1
        | otherwise = 0
    fromInteger n = F n 1

--1f
fun:: Frac -> [Frac]->[Frac]
fun _ [] = []
fun f (f1:fs)
    | f1 > 2*f = f1: fun f fs
    | otherwise = fun f fs

--2
data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

--2a
instance (Num a, Show a) => Show (Exp a) where
    show e = infixa e
        where
            infixa (Const a) = show a
            infixa (Simetrico e) = "-" ++ infixa e
            infixa (Mais e1 e2) = infixa e1 ++ " + " ++ infixa e2
            infixa (Menos e1 e2) = infixa e1 ++ " - " ++ infixa e2
            infixa (Mult e1 e2) = infixa e1 ++ " x " ++ infixa e2

--2b
instance (Num a, Eq a) => Eq (Exp a) where
    e1 == e2 = (infixa e1) == (infixa e2)
        where
            infixa (Const a) = a
            infixa (Simetrico e) = -(infixa e)
            infixa (Mais e1 e2) = infixa e1 + infixa e2
            infixa (Menos e1 e2) = infixa e1 - infixa e2
            infixa (Mult e1 e2) = infixa e1 * infixa e2

--2c
instance Num a => Num (Exp a) where
    e1 + e2 = Mais e1 e2
    e1 - e2 = Menos e1 e2
    e1 * e2 = Mult e1 e2
    negate e = Simetrico e
    abs e = e * signum e
    signum e = Const (signum (infixa e))
        where
            infixa (Const a) = a
            infixa (Simetrico e) = -(infixa e)
            infixa (Mais e1 e2) = infixa e1 + infixa e2
            infixa (Menos e1 e2) = infixa e1 - infixa e2
            infixa (Mult e1 e2) = infixa e1 * infixa e2
    fromInteger a = Const (fromInteger a)
 
 --3
data Movimento = Credito Float | Debito Float 
data Data = D Int Int Int
data Extracto = Ext Float [(Data,String,Movimento)]

--3a
instance Eq Data where
    (D a m d) == (D a1 m1 d1) = d==a1 && m==m1 && d==d1
instance Ord Data where
    (D a m d) <= (D a1 m1 d1) = a<=a1 && m<=m1 && d<=d1

--3b
instance Show Data where
    show (D a m d) = (show a) ++ "/" ++ (show m) ++ "/" ++ (show d)

--3c
ordena:: Extracto->Extracto
ordena (Ext e l) = Ext e (ord l)
    where
        ord [] = []
        ord ((d,s,m):hs) = inserta (d,s,m) (ord hs)
            where
                inserta (d,s,m) [] = [(d,s,m)]
                inserta (d,s,m) ((d1,s1,m1):hs) 
                    | d <= d1 = (d,s,m):(d1,s1,m1):hs
                    | otherwise = (d1,s1,m1): (inserta (d,s,m) hs)

--3d
calc (Ext f []) = f 
calc (Ext f ((_,_,Credito e):t)) = calc (Ext (f+e) t)
calc (Ext f ((_,_,Debito e):t)) = calc (Ext (f-e) t)
instance Show Extracto where
    show (Ext f l) =
        "Saldo anterior: " ++ show f ++ "\n" ++
        "---------------------------------------" ++ "\n" ++
        "Data Descricao Credito Debito" ++ "\n" ++
        (s l) ++ "\n" ++
        "---------------------------------------" ++ "\n" ++
        "Saldo actual: " ++ show (calc (Ext f l))
            where
                s ((d,st,Credito e):t) = (show d) ++ " " ++ st ++ " " ++ (show e) ++ "\n" ++ (s t)
                s ((d,st,Debito e):t) = (show d) ++ " " ++ st ++ "         " ++ (show e) ++ "\n" ++ (s t)


