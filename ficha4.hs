
--1

--1a
--[6,12,18]
m6:: [Int]->[Int]
m6 [] = []
m6 (h:t)
    | (mod h 2 == 0) && (mod h 3 == 0) = h:m6 t
    | otherwise = m6 t
    