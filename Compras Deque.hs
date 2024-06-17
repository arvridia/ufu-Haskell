enqueue :: String -> [String] -> [String]
enqueue elem l = l ++ [elem]

peek :: [String] -> String
peek l = head l

dequeue :: [String] -> [String]
dequeue l = tail l 

enqueue' :: a -> [a] -> [a]
enqueue' elem elems = elems ++ [elem]

peek' :: [a] -> a
peek' l = head l

dequeue' :: [a] -> [a]
dequeue' l = tail l 

ord ::(Ord a) => a -> [a] -> [a]
ord elem [] = [elem]
ord elem (y:ys)
 | elem <= y = elem:y:ys
 | otherwise = y: (ord elem ys)

ordena ::(Ord a) => [a] -> [a]
ordena [] = []
ordena (elem:elems) = ord elem (ordena elems)

enqueue'' ::(Ord a) => a -> [a] -> [a]
enqueue'' elem elems = ordena (elems ++ [elem])
  
peek'' :: [a] -> a
peek'' l = head l

dequeue'' :: [a] -> [a]
dequeue'' l = tail l 


type Quantidade = Float
type Nome = String
data Unidade = Litro | Grama | Dúzia | Mililitro | Kilograma | Metro | Vazio deriving (Eq, Ord, Show)
data ListaDeCompras = Compra Nome Quantidade Unidade deriving (Eq, Ord, Show)

organiza :: [ListaDeCompras] -> [String]
organiza [] = []
organiza (Compra x y z: xs) = if y >= 2 then (show x ++ " x " ++ show y ++ " " ++ show z ++ "s"):(organiza xs)
                                        else (show x ++ " x " ++ show y ++ " " ++ show z):(organiza xs)


total :: Unidade -> [ListaDeCompras] -> Float
total u [] = 0.0
total u l@(Compra x y z: xs) 
 | z == u = y + (total u xs)
 | otherwise = total u xs