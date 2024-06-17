import Data.Char (isDigit, isAlpha)
--Arthur Resende Santos 12011BCC020
--"$" para evitar uso excessivo de parenteses
--Exercicio 1
--a)
numof c list = length $ filter (==c) list

--b)
ellen :: [String] -> [Int]
ellen list = map length list

--c)
ssp :: [Int] -> Int
ssp list = foldr (+) 0 $ map (^2) $ filter (>=0) list

--Arthur Resende Santos
--Exercicio 2 
separa :: String -> (String, String)
separa s = (filter isAlpha s, filter isDigit s)

--Arthur Resende Santos
--Exercicio 3
--a)
const :: a -> b -> a
const x y = x
--esta funcao retorna o valor do primeiro argumento, 
--independente do segundo argumento passado, uma constante

--b)
swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)
-- esta funcao inverte a posicao das variaveis, 
--o primeiro elemento da tupla é retornado como o segundo e o segundo como primeiro

--c) 
apply :: (a -> a) -> a -> a
apply f x = f x
--esta funcao aplica a funcao passada como primeiro argumento
--na variavel do segundo argumento e retorna o resultado

--d) 
flip :: (a -> a -> a) -> a -> a -> a
flip f x y = f y x
--esta funcao aplica uma funcao passada como primeiro argumento
--no entanto, a funcao é aplicada nas variaveis do segundo e terceiro argumento
--em ordem invertida, é aplicada para a segunda variavel como primeiro argumento e
--e a primeira como segundo

--Arthur Resende Santos
--Exercicio 4
--a)
data Info = Info {nome::String, quantidade::Float, preco::Float}

--b)
valorItem :: Info -> Float
valorItem x = quantidade x * preco x

calculo :: [Info] -> Float
calculo x = foldr (+) 0 $ map valorItem x


