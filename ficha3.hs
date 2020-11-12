
data Ponto = C Double Double 
            | P Double Double deriving (Show, Eq)

toP:: Ponto->Ponto
toP (C x y) = (P raio angulo)
    where
        raio = dist (C 0 0) (C x y)
        angulo = acos(x/raio)
toP p = p

toC:: Ponto->Ponto
toC (P r a) = (C x y)
    where
        x = r * cos a
        y = r * sin a
toC p = p

posx:: Ponto->Double
posx (C x _) = x
posx p = posx (toC p)

posy:: Ponto->Double
posy (C _ y) = y
posy p = posy (toC p)

raio:: Ponto->Double
raio (P r _) = r
raio p = raio (toP p)

angulo:: Ponto->Double
angulo (P _ a) = a
angulo p = angulo (toP p)

dist:: Ponto->Ponto->Double
dist p1 p2 = sqrt((posx p2-posx p1)^2 + (posy p2-posy p1)^2)

data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show, Eq)

poligono:: Figura->Bool
poligono (Rectangulo _ _) = True
poligono (Triangulo _ _ _) = True
poligono _ = False

vertices:: Figura->[Ponto]
vertices (Rectangulo p1 p2) = [p1, p2]
vertices (Triangulo p1 p2 p3) = [p1, p2, p3]
vertices _ = []

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = dist p1 p2
        b = dist p2 p3
        c = dist p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Rectangulo (C x1 y1) (C x2 y2)) = (abs y2-y1)*(abs x2-x1)
area (Rectangulo p1 p2) = area (Rectangulo (toC p1) (toC p2))
area (Circulo _ r) = r^2 * pi
area _ = -1

perimetro:: Figura->Double
perimetro (Triangulo p1 p2 p3) = dist p1 p2 + dist p1 p3 + dist p2 p3
perimetro (Rectangulo (C x1 y1) (C x2 y2)) = 2*(abs y2-y1) + 2*(abs x2-x1)
perimetro (Rectangulo p1 p2) = perimetro (Rectangulo (toC p1) (toC p2))
perimetro (Circulo _ r) = 2*r*pi
perimetro _ = -1

-----------------------------------------------
type Poligonal = [Ponto]

--2a
comprimento::Poligonal->Int
comprimento (p1:p2:ps) = fromEnum(dist p1 p2) + comprimento (p2:ps)
comprimento _ = 0

--2b

--2c
triangula::Poligonal->[Figura]
triangula (p0:p1:p2:_:[]) = [(Triangulo p0 p1 p2)]
triangula (p0:p1:p2:p3:ps) = (Triangulo p0 p1 p2) : triangula (p0:p2:p3:ps)
triangula _ = []

--2d
areaLinha::Poligonal->Double
areaLinha (p0:p1:p2:p3:ps) = aux (triangula (p0:p1:p2:p3:ps))
    where
        aux (h:t) = area h + aux t
areaLinha _ = 0

--2e
mover:: Poligonal->Ponto->Poligonal
mover [] _ = []
mover (p:ps) pn = pn:move (posx pn-posx p, posy pn - posy p) ps
    where
        move (x,y) (h:t) = C (posx h + x) (posy h +y):move(x,y) t

--2f
zoom:: Double->Poligonal->Poligonal
zoom _ [] = []
zoom _ [x] = [x]
zoom n (p1:p2:ps) = p1: zoom n (mover (p2:ps) pn)
    where
        pn = C (n*posx p2 - posx p1*(n-1)) (n*posy p2 - posy p1*(n-1))


-------------------------------------------------------
--3
data Contacto = Casa Integer
            |   Trab Integer
            |   Tlm Integer
            |   Email String
            deriving (Show,Eq)
type Nome = String
type Agenda = [(Nome, [Contacto])]

--3a
acrescEmail:: Nome->String->Agenda->Agenda
acrescEmail nome email [] = [(nome, [Email email])]
acrescEmail nome email ((n,c):t)
    | nome == n = (n, aux email c):t
    | otherwise = (n,c):acrescEmail nome email t
        where
            aux email [] = [Email email]
            aux email (Email e:t)
                | e == email = Email e:t
                | otherwise = (Email e):aux email t
            aux email (c:t) = c: aux email t

--3b
verEmails:: Nome->Agenda->Maybe [String]
verEmails nome ((n,c):t)
    | nome == n = Just (aux c)
    | otherwise = verEmails nome t
        where
            aux (Email e:t) = e:(aux t)
            aux (_:t) = aux t
            aux [] = []
verEmails _ [] = Nothing

--3c
consTelefs:: [Contacto]->[Integer]
consTelefs ((Tlm tl):t) = tl:consTelefs t
consTelefs ((Trab tl):t) = tl:consTelefs t
consTelefs ((Casa tl):t) = tl:consTelefs t
consTelefs (_:t) = consTelefs t
consTelefs [] = []

--3d
casa:: Nome->Agenda->Maybe Integer
casa nome ((n,c):t) 
    | nome == n = Just (aux c)
    | otherwise = casa nome t
        where
            aux (Casa c:_) = c
            aux (_:t) = aux t
casa _ [] = Nothing

-------------------------------------------

--4
type Dia = Int
type Mes = Int
type Ano = Int
--type Nome = String

data Data = D Dia Mes Ano deriving Show

type TabDN = [(Nome, Data)]

--4a
procura:: Nome->TabDN->Maybe Data
procura nome ((n,d):t)
    | nome == n = Just d
    | otherwise = procura nome t
procura _ [] = Nothing

--4b
idade:: Data->Nome->TabDN->Maybe Int
idade (D d1 m1 a1) nome ((n,D d2 m2 a2):t)
    | n == nome = if (a1>a2 && m1>m2) || (a1>a2 && m1>=m2 && d1>=d2) then Just (a1-a2) else Just (a1-a2-1)
    | otherwise = idade (D d1 m1 a1) nome t
idade _ _ [] = Nothing

--4c
anterior:: Data->Data->Bool
anterior (D d1 m1 a1) (D d2 m2 a2) = (a1<a2) || (a1==a2 && m1<m2) || (a1==a2 && m1==m2 && d1<d2)

--4d
ordena:: TabDN->TabDN
ordena [] = []
ordena [(n,d)] = [(n,d)]
ordena ((n,d):t) = insert' (n,d) (ordena t)
    where
        insert' (n,d) [] = [(n,d)]
        insert' (n,d) ((n1,d1):t) 
            | anterior d d1 = (n,d):(n1,d1):t 
            | otherwise = (n1,d1):insert' (n,d) t

--4e
porIdade:: Data->TabDN->[(Nome,Int)]
porIdade _ [] = []
porIdade d tbdn = aux d tbdn []
    where 
        aux:: Data->TabDN->[(Nome,Int)]->[(Nome,Int)]
        aux _ [] acc = acc
        aux d1 ((nome,d2):t1) ((n,i):t2)
            | i' <= i = aux d1 t1 ((nome,i'):(n,i):t2)
            | otherwise = aux d1 t1 ((n,i):(nome,i'):t2)
                where
                    Just i' = idade d1 n [(nome,d2)]

---------------------------------------------------------------------------------------------

--5

data Movimento = Credito Float | Debito Float deriving Show
data Data5 = D5 Int Int Int deriving Show
data Extracto = Ext Float [(Data, String, Movimento)] deriving Show

--5a

