import Data.List ( nub )
data RTree a = R a [RTree a] deriving (Show, Eq)

paths:: RTree a -> [[a]]
paths (R x []) = [[x]]
paths (R x [h]) = map (x:) (paths h)  
paths (R x (h:t)) = paths (R x [h]) ++ paths (R x t)

unpaths:: Eq a => [[a]]->RTree a
unpaths [[x]] = R x []
unpaths list = R x [unpaths (foldl (\acc a -> if y == head a then a:acc else acc) [] l1) | y <- l2]
    where
        x = head (head list)    
        l1 = [tail x | x <- list]
        l2 = nub [head x | x <- l1]

data Contacto = Casa Integer 
                | Trab Integer
                | Tlm Integer
                | Email String deriving Show
type Nome = String
type Agenda = [(Nome,[Contacto])]

consultaIO:: Agenda->IO()
consultaIO a =  do  putStrLn "Introduza o Nome"
                    x <- getLine 
                    aux x a
    where
        aux:: Nome->Agenda->IO()
        aux x [] = do putStrLn "----"
        aux x ((n,l):t)
            | x == n = s l
            | otherwise = aux x t
                where
                    s (h:t) = case h of 
                        Casa i -> do putStrLn ("Casa: " ++ show i)
                        Trab i -> do putStrLn ("Trabalho: " ++ show i)
                        Tlm i -> do putStrLn ("Telemóvel: " ++ show i)
                        Email e -> do putStrLn ("Email: " ++ e)