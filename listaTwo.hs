--Arthur Resende Santos 12011BCC020 
--Exercicio 1
--a)
ordena2 :: Int -> Int -> (Int, Int)
ordena2 a b =   
	if a >= b 
	then (b,a)
	else (a,b)

--b)
ordena2' :: Int -> Int -> (Int, Int)
ordena2' a b
 | a >= b = (b,a)
 | otherwise = (a,b)

--Arthur Resende Santos 12011BCC020
--Exercicio 2
--a)
dizMes :: Int -> String
dizMes a 
 | a == 1  = "Janeiro"
 | a == 2  = "Fevereiro"
 | a == 3  = "Marco"
 | a == 4  = "Abril"
 | a == 5  = "Maio"
 | a == 6  = "Junho"
 | a == 7  = "Julho"
 | a == 8  = "Agosto"
 | a == 9  = "Setembro"
 | a == 10 = "Outubro"
 | a == 11 = "Novembro"
 | a == 12 = "Dezembro"
 | otherwise = error "Erro!"

 --b)
dizMes' :: Int -> String
dizMes' a =  
 if a == 1  then "Janeiro"
 	else if a == 2  then "Fevereiro"
 	else if a == 3  then "Marco"
 	else if a == 4  then "Abril"
 	else if a == 5  then "Maio"
 	else if a == 6  then "Junho"
 	else if a == 7  then "Julho"
 	else if a == 8  then "Agosto"
 	else if a == 9  then "Setembro"
 	else if a == 10 then "Outubro"
 	else if a == 11 then "Novembro"
 	else if a == 12 then "Dezembro"
 	else = error "Erro!"
--Sim, eh possivel utilizar "if then else" nesse caso, mas nao eh interessante
--visto que ao utilizar guardas o codigo fica bem mais enxuto
--e podemos escrever bem menos, o que eh bem mais interessante
--eh melhor usar guardas nesse tipo de caso, quando ha muitas opcoes

--Arthur Resende Santos 12011BCC020
--Exercicio 3 
triangulo :: Float -> Float -> Float -> String
triangulo x y z
 | ehPositivo && ehTriangulo && x==y && y==z = "Eh triangulo e eh um triangulo equilatero
 | ehPositivo && ehTriangulo && (x==y || y==z || x==z) = "Eh triangulo e eh um triangulo isosceles!"
 | ehPositivo && ehTriangulo && x\=y && y\=z && x\=z = "Eh triangulo e eh um triangulo escaleno!"
 where ehPositivo  = x >=0 && y >=0 && z >=0
 	   ehTriangulo = x+y > z || y+z > x || z+x > y