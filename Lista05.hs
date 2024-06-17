import Data.Char

-- Davi de Pontes Pasquini Exercício 1 - a

numof :: Char -> String -> Int
numof a b
  | null b    = 0
  | otherwise = length (filter (\x -> head x == a) [b]) + numof a (tail b)

-- Davi de Pontes Pasquini Exercício 1 - b

ellen :: [String] -> [Int]
ellen a
  | null a    = []  
  | otherwise = map length a

-- Davi de Pontes Pasquini Exercício 1 - c

ssp :: [Int] -> Int
ssp a
  | null a    = 0
  | otherwise = foldr (+) 0 (map (^2) (filter (>= 0) a))

-- Davi de Pontes Pasquini Exercício 2

separa :: String -> (String, String)
separa a
  | null a    = ("", "")
  | otherwise = (filter isAlpha a, filter isDigit a)

-- Davi de Pontes Pasquini Exercício 3

{-
a) const x y = x
const :: x -> y -> x

Retorna o primeiro parâmetro passado.

--b) swap (x,y) = (y,x)
swap :: (x, y) -> (y, x)

Inverte os parâmetros de um par ordenado

--c) apply f x = f x
apply :: f -> x -> fx

Aplica a função f em uma variável x

--d) flip f x y = f y x
flip :: f -> x -> y -> fyx

Inverte a ordem dos parâmetros de uma função
-}

-- Davi de Pontes Pasquini Exercício 4 - a

data ShopItem = ShopItem {nome :: String, quantidade :: Float, preco :: Float}

-- Davi de Pontes Pasquini Exercício 4 - b

valorItem :: ShopItem -> Float
valorItem a = preco a * quantidade a

valorTotal :: [ShopItem] -> Float
valorTotal a = foldr (+) 0 $ map (valorItem) a