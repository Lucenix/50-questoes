
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
mover l p 
    | dist p x > dist p y = p:l
    | otherwise = p:(reverse l)
    where
        (x,y) = aux l
            where 
                aux (h:t) = (h,last t)

--2f
zoom:: Double->Poligonal->Poligonal
zoom _ [x] = [x]
zoom n (h:t) = h: aux n t
    where
        aux _ [x] = (x:[])
        aux n (p0:p1:ps) = (C (n*posx p1 - posx p0) (n*posy p1 - posy p0)):aux n (p1:ps)

-------------------------------------------------------
--3
data Contacto = Casa Integer
            |   Trab Integer
            |   Tlm Integer
            |   Email String
            deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

--3a
acrescEmail:: Nome->String->Agenda->Agenda
acrescEmail nome email agenda = ((nome, [Email email]):agenda)

--3b
--verEmails:: Nome->Agenda->Maybe [String]