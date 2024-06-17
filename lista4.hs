import Data.Char

--Arthur Resende Santos 12011BCC020
--Exercicio 1

addPares :: [(Int, Int)] -> [Int]
addPares l = [(fst a)+ (snd a) | a <- l]

--Arthur Resende Santos 12011BCC020
--Exercicio 2
--a)
convMaius :: [Char] -> [Char]
convMaius [] = []
convMaius (x:xs) = toUpper(x):convMaius xs

--Arthur Resende Santos 12011BCC020
--Exercicio 2
--b)

tuple :: [Char] -> ([Char], [Char])
tuple x = (convMaius x, x)

--Arthur Resende Santos 12011BCC020
--Exercicio 3

getPos :: [Int] -> Int -> Int -> Int
getPos (x:xs) y z 
 |x == y = z
 |otherwise = getPos xs y z+1

maior :: [Int] -> (Int, Int)
maior x = (maximum x, (getPos x (maximum x) 1))

--Arthur Resende Santos 12011BCC020
--Exercicio 4

removeElem :: [Int] -> Int -> Int -> [Int]
removeElem x y z
  | x == [] = []
  | z == y = tail x
  | otherwise = (head x):(removeElem (tail x) y (z + 1))

--Arthur Resende Santos 12011BCC020
--Exercício 5

type NomeAluno = String
type MediaNota = Int
type Aluno = (NomeAluno, MediaNota)

type Turma = [Aluno]
										 
aprovados l d = [fst x | x <- l, (snd x) > d]