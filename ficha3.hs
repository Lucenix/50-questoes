
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

type Poligonal = [Ponto]

-----------------------------------------------

--a
comprimento::Poligonal->Int
comprimento (p1:p2:ps) = fromEnum(dist p1 p2) + comprimento (p2:ps)
comprimento _ = 0

--b
