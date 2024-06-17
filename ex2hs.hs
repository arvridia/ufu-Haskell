import BaseGrafo

caminho :: Int -> [(Int,Int)] -> [Int]
caminho x [] = []
caminho x ((y,z):l)
 | x == y = z : caminho x l
 | otherwise = caminho x l