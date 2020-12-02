
import Data.Char
--1

--1a
--[6,12,18]
--[6,12..20]
m6:: [Int]->[Int]
m6 [] = []
m6 (h:t)
    | (mod h 2 == 0) && (mod h 3 == 0) = h:m6 t
    | otherwise = m6 t

--1b
--[6,12,18]
--[6,12..20]

--1c
--[(10,20),(11,19),(12,18)..(20,10)]
--[(x,y) | x <- [0..20], y <- [0..20], x+y == 30]
--[(x,y) | x<-[10..20], y<-[30-x]]

--1d
--[1,1,4,4,9,9,16,16,25,25]
--[x^2 | x<-[1..5], y<-[1..5]]
--saltei à frente

--2
--2a
--[2^x | x<-[0..10]]

--2b
--[(x,y) | x<-[1..5], y<-[1..5], x+y == 6]

--2c
--[[x | x<-[1..y]], y<-[1..5]]

--2d
--[[1 | x<-[1..y], y<-[1..5]] 

--2e
--[fact x | x <- [1..6]]

--3
digitAlpha':: String->(String,String)
digitAlpha' [] = ([],[])
digitAlpha' (h:t)
    | isAlpha h = (h:a,b)
    | isDigit h = (a,h:b)
    | otherwise = (a,b)
        where (a,b) = digitAlpha' t

--4
nzp:: [Int]->(Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t)
    | h<0 = (1+a,b,c)
    | h==0 = (a,1+b,c)
    | otherwise = (a,b,c+1)
        where (a,b,c) = nzp t

--5
divMod':: Integral a => a->a->(a,a)
divMod' x y
    | (x>0 && y<0) || (x<0 && y>0) = (q1-1,r1)
    | x>0 && y>0 && x<y = (0,x)
    | x<0 && y<0 && x>y = (0,x)
    | otherwise = (q2+1,r2)
        where 
            (q1,r1) = divMod' (x+y) y
            (q2,r2) = divMod' (x-y) y

--6
fromDigits:: [Int]->Int
fromDigits [] = 0
fromDigits (h:t) = aux t h 
    where
        aux [] acc = acc
        aux (h:t) acc = aux t (h+10*acc)

--7
maxSumInit:: (Num a, Ord a)=>[a]->a
maxSumInit (h:t) = aux t h h
    where
        aux [] max _ = max
        aux (h:t) max total
            | (total+h)>max = aux t (total+h) (total+h)
            | otherwise = aux t max (total+h)

--8
fib:: Int->Int
fib n = aux n 1 1
    where
        aux 1 en _ = en
        aux n en en1 = aux (n-1) en1 (en+en1)

fib' :: Int -> Int
fib' 0 = 0
fib' 1 = 1
fib' n = fib' (n-1) + fib' (n-2)