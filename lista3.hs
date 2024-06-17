--Arthur Resende Santos
--Exercicio 1
imprimeNVezes :: Int -> IO ()
imprimeNVezes 1 = putStrLn "Frase"
imprimeNVezes n = if n > 1 then putStrLn "Frase" >> imprimeNVezes (n-1)
                     else putStrLn "Insira um numero maior que 0"

--Arthur Resende Santos
--Exercicio 2
eLogico :: Bool -> Bool -> Bool
eLogico a b 
 | a == True && b == True = True
 | otherwise = False
 
--Arthur Resende Santos
--Exercicio 3
eLogico2 :: Bool -> Bool -> Bool
eLogico2 a b 
 | a == True = b
 | otherwise = False
 
--Arthur Resende Santos
--Exericio 4
comb :: Int -> Int -> Int
comb n 1 = n
comb n k
 | n == k = 1
 | (n > k && k > 1) = comb (n-1) (k-1) + comb (n-1) k 
 | otherwise = error "insira numeros validos para a operacao!"
 