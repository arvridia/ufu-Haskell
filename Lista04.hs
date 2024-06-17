import Data.Char

-- Davi de Pontes Pasquini - Exercício 1

addPares :: [(Int, Int)] -> [Int]
addPares [] = []
addPares ((a, b):xs) = (a + b):addPares xs

-- Davi de Pontes Pasquini - Exercício 2 a

recuMaiuscula :: [Char] -> [Char]
recuMaiuscula [] = []
recuMaiuscula (x:xs) = toUpper(x):recuMaiuscula xs

-- Davi de Pontes Pasquini - Exercício 2 b

recuTupla2 :: [Char] -> ([Char], [Char])
recuTupla2 x = (recuMaiuscula x, x)

-- Davi de Pontes Pasquini - Exercício 3

retornaPosicao :: [Int] -> Int -> Int -> Int
retornaPosicao (x:xs) y z = if x == y then z
                                    else retornaPosicao xs y z+1

exercicio3 :: [Int] -> (Int, Int)
exercicio3 x = (maximum x, (retornaPosicao x (maximum x) 1))

-- Davi de Pontes Pasquini - Exercício 4

removeElemento :: [Int] -> Int -> Int -> [Int]
--removeElemento lista posicao CONTADOR (foi necessário professora)
removeElemento x y z
  | x == [] = []
  | z == y = tail x
  | otherwise = (head x):(removeElemento (tail x) y (z + 1))

-- Davi de Pontes Pasquini - Exercício 5

type NomeAluno = String
type MediaNota = Int
type Aluno = (NomeAluno, MediaNota)
type Turma = [Aluno]

getNota (_, a) = a 

retornaAprovados :: Turma -> Int -> Turma
retornaAprovados x y
  | x == [] = []
  | otherwise = if getNota (head x) >= y then (head x):(retornaAprovados (tail x) y)
                                         else retornaAprovados (tail x) y