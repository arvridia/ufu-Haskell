--Arthur Resende Santos 12011BCC020
--Trabalho 2 Final

import System.IO
type FilePath = String

type Codigo = Int
type Nome = String 
type Preco = Float
type Produto = (Codigo, Nome, Preco)

tabelaProdutos :: [Produto]
tabelaProdutos = [(001, "Chocolate", 5.25)
                 ,(002, "Biscoito", 10.10)
                 ,(003, "Laranja", 4.60)
                 ,(004, "Sabao", 2.10)
                 ,(005, "Batata Chips", 6.90)
                 ,(006, "Doritos", 8.90)]

isCodigo :: Codigo -> Produto -> Bool
isCodigo c (p1, p2, p3) = p1 == c

getPreco :: Produto -> Preco
getPreco (p1, p2, p3) = p3

getNome :: Produto -> Nome
getNome (p1, p2, p3) = p2

buscaNomePorCodigo' :: Codigo -> [Produto] -> Nome
buscaNomePorCodigo' _ [] = error "Nao ha este produto"
buscaNomePorCodigo' c (x:xs)
 | isCodigo c x = getNome x 
 | otherwise = buscaNomePorCodigo' c xs
 
buscaNomePorCodigo :: Codigo -> Nome
buscaNomePorCodigo c = buscaNomePorCodigo' c tabelaProdutos

buscaPrecoPorCodigo' :: Codigo -> [Produto] -> Preco
buscaPrecoPorCodigo' _ [] = error "Nao ha este produto"
buscaPrecoPorCodigo' c (x:xs)
 | isCodigo c x = getPreco x 
 | otherwise = buscaPrecoPorCodigo' c xs
 
buscaPrecoPorCodigo :: Codigo -> Preco
buscaPrecoPorCodigo c = buscaPrecoPorCodigo' c tabelaProdutos

calculaPrecos :: [Codigo] -> Float 
calculaPrecos list = foldr (+) 0 $ map buscaPrecoPorCodigo list

pegaTamanho :: Codigo -> Int
pegaTamanho c = 30 - (length (buscaNomePorCodigo c ++ show (buscaPrecoPorCodigo c)))

pegaTamanho' :: [Codigo] -> Int
pegaTamanho' list = 30 - (length ("Total:" ++ show (calculaPrecos list)))

--nao sei porque mas esse "\n" nao esta pulando linha...
formataStrProduto :: Codigo -> String
formataStrProduto c = buscaNomePorCodigo c ++ replicate (pegaTamanho c) '.' ++ show (buscaPrecoPorCodigo c) ++ "\n"

geraNotaFiscal :: [Codigo] -> IO ()
geraNotaFiscal list = writeFile "arquivoTrab2.txt" (show (map formataStrProduto list) ++ "\n" ++ "Total:" ++ replicate (pegaTamanho' list) '.' ++ show (calculaPrecos list))




