explosão :: Tabuleiro -> Célula -> Tabuleiro
explosão t c = explodeCélula Célula

poeJogador :: Linha -> NumCel -> Linha 
poeJogador l i = 

linha coluna

modificaTab :: Tabuleiro -> Localização -> Int -> Tabuleiro
modificaTab t l i = if i == 0 then t
							      else if i == 1 then explosão (pegaCoordenada t l)
							                     else if i == 2 then retiraPresente (pegaCoordenada t l)
											    else poeBomba (pegaCoordenada t l)


explosão :: Célula -> Célula
explosão (P,(x,y)) = if (last P == Jogador || last P == Parede || last P == Presente) then (reverse (tail (reverse P)), (x,y))
					                                                                  else (P, (x,y))

retiraPresente :: Célula -> Célula
retiraPresente (P,(x,y)) = (reverse (tail (reverse P)), (x,y))

poeBomba :: Célula -> Célula
poeBomba (P,(x,y)) = ((Bomba:P), (x,y))





--coloca a bomba e depois da proxima instrucao ela explode

--precisa de um jeito eficiente de mexer no tabuleiro sem ter que passar ele inteiro na chamada de cada função
data Presente = Patins | Arremesso 
data Item = Grama | Presente | Pedra | Parede | Bomba | Jogador
type Localizaçao = (Int, Int) -- (Linha, Coluna)
data Identificador = X | Y | Z
type Capacidade = ((Patins, Int), (Bomba, Int), (Arremesso, Int))
data Direção = N | S | L | O
data Movimento = Esquerda | Direita | Cima | Baixo --

--type listaJogadores = [Jogador, Jogador, Jogador, Jogador]
--type listaJogadores = [(A, _, _, _), (B, _, _, _), (C, _, _, _), (D, _, _, _)]

type Jogador = (Identificador, Localização, Capacidade, Direção) 

type Instrução = (Identificador, Movimento) --

type Pilha = [Item]
type NumCel = Int
type Célula = (Pilha, NumCel)
type Linha = (Célula, Célula, Célula, Célula, Célula, Célula, Célula, Célula)
type Tabuleiro = (Linha, Linha, Linha, Linha, Linha, Linha, Linha, Linha)

type Pilha1 = [Pedra]
type Pilha2 = [Grama]
type Pilha3 = [Grama, Presente]
type Pilha4 = [Parede]
type Pilha5 = [Grama, Parede]
type Pilha6 = [Grama, Presente, Parede]
type Pilha7 = [Grama, Bomba]
type Pilha8 = [Grama, Jogador]
type PilhaVazia = []

data tPilha = Pilha Pilha1 | Pilha Pilha2 | Pilha Pilha3 | Pilha Pilha4 | Pilha Pilha5 | Pilha Pilha6 | Pilha Pilha7 | Pilha Pilha8 | Pilha PilhaVazia

--cria tabuleiro pre definido
criaTabuleiro :: Tabuleiro -> Tabuleiro
criaTabuleiro t = 	  (((Pilha1, 1), (Pilha1, 2), (Pilha1, 3), (Pilha1, 4), (Pilha1, 5), (Pilha1, 6), (Pilha1, 7), (Pilha1, 8)),
		          ((Pilha1, 9), (Pilha8, 10), (Pilha2, 11), (Pilha5, 12), (Pilha2, 13), (Pilha3, 14), (Pilha8, 15), (Pilha1, 16)),
		          ((Pilha1, 17), (Pilha2, 18), (Pilha1, 19), (Pilha3, 20), (Pilha1, 21), (Pilha2, 22), (Pilha4, 23), (Pilha1, 24)),
		          ((Pilha1, 25), (Pilha2, 26), (Pilha2, 27), (Pilha4, 28), (Pilha2, 29), (Pilha5, 30), (Pilha2, 31), (Pilha1, 32)),
		          ((Pilha1, 33), (Pilha3, 34), (Pilha1, 35), (Pilha2, 36), (Pilha1, 37), (Pilha2, 38), (Pilha2, 39), (Pilha1, 40)),
		          ((Pilha1, 41), (Pilha6, 42), (Pilha2, 43), (Pilha5, 44), (Pilha2, 45), (Pilha3, 46), (Pilha6, 47), (Pilha1, 48)),
		          ((Pilha1, 49), (Pilha8, 50), (Pilha2, 51), (Pilha2, 52), (Pilha2, 53), (Pilha2, 54), (Pilha8, 55), (Pilha1, 56)),
		          ((Pilha1, 57), (Pilha1, 58), (Pilha1, 59), (Pilha1, 60), (Pilha1, 61), (Pilha1, 62), (Pilha1, 63), (Pilha1 ,64)))



-----------------------------

-- Recebe uma linha e o indice da celula que vai ser devolvida
pegaColuna :: Linha -> Int -> Celula
pegaColuna (coluna1, coluna2, coluna3, coluna4, coluna5, coluna6, coluna7, coluna8) c
  | c == 1 = coluna1
  | c == 2 = coluna2
  | c == 3 = coluna3
  | c == 4 = coluna4
  | c == 5 = coluna5
  | c == 6 = coluna6
  | c == 7 = coluna7
  | c == 8 = coluna8
  | otherwise = error "Coluna inválida"


pegaLinha :: Tabuleiro -> Int -> Linha
pegaLinha (linha1, linha2, linha3, linha4, linha5, linha6, linha7, linha8) l
  | l == 1 = linha1
  | l == 2 = linha2
  | l == 3 = linha3
  | l == 4 = linha4
  | l == 5 = linha5
  | l == 6 = linha6
  | l == 7 = linha7
  | l == 8 = linha8
  | otherwise = error "Linha inválida"


pegaCoordenada :: Tabuleiro -> (Int, Int) -> Celula
pegaCoordenada t (l, c) = pegaColuna (pegaLinha t l) c

-------------------------

movimentaJogador :: Tabuleiro -> Movimento -> Tabuleiro
movimentaJogador t m = if (movimentoPossivel) == 1 then modificaTabuleiro t m
                                                   else t
					
movimentaJogador :: Tabuleiro -> Jogador -> Instrução -> Tabuleiro
movimentaJogador t j i
		if (movimentoPossivel t j i) == 1 then = modificaTabuleiro t j i
                else = t

--ve pra onde vai, onde esta, atualiza a pilha em que estava e que vai estar, atualiza info do player
--provavelmente precisa de uma função pra atualizar a pilha e outra pra ver se tinha item e atualizar o player
modificaTabuleiro :: Tabuleiro -> Jogador -> Instrução -> Tabuleiro
modificaTabuleiro t j i
		if i == (esquerda, a) && j == (a, (x, y), _, _) then pegaCoordenada t (x, y - 1) = Pilha8 && pegaCoordenada t (x, y) = Pilha2 && j = (a, (x, y - 1), _, O)
		else if i == (direita, a) && j == (a, (x, y), _, _) then pegaCoordenada t (x, y + 1) = Pilha8 && pegaCoordenada t (x, y) = Pilha2 && j = (a, (x, y + 1), _, L)
		else if i == (cima, a) && j == (a, (x, y), _, _) then pegaCoordenada t (x - 1, y) = Pilha8 && pegaCoordenada t (x, y) = Pilha2 && j = (a, (x - 1, y), _, N)
		else if i == (baixo, a) && j == (a, (x, y), _, _) then pegaCoordenada t (x + 1, y) = Pilha8 && pegaCoordenada t (x, y) = Pilha2 && j = (a, (x + 1, y), _, S)
		
		
--                             (Linha, Coluna)                 
--type Jogador = (Identificador, Localização, Capacidade, Direção)
--type Instrução = (Identificador, Movimento)

movimentoPossivel :: Tabuleiro -> Jogador -> Instrução -> Int
movimentoPossivel t j i
                  | i == (esquerda, a) && j == (a, (x, y), _, _) && achaLocal (x, y - 1) t == 1 = 1
                  | i == (direita, a) && j == (a, (x, y), _, _) && achaLocal (x, y + 1) t == 1 = 1
                  | i == (cima, a) && j == (a, (x, y), _, _) && achaLocal (x - 1, y) t == 1 = 1
                  | i == (baixo, a) && j == (a, (x, y), _, _) && achaLocal (x + 1, y) t == 1 = 1
		  | otherwise = 0

--achaLocal ja recebe o local para o qual o jogador VAI e retorna 1 se tiver como o jogador IR ou 0 se nao tiver como
achaLocal :: Localização -> Tabuleiro -> Int
achaLocal (x, y) t
	pegaCoordenada t (x, y) = c
	if c == Pedra || c == Parede then = 0
	else = 1

{-
exemplo: achaLocal (1, 2) t
		lx (e1, e2, e3, e4, e5, e6, e7, e8) = e2
		if e2 == Pedra || e2 == Parede then = 0
		else = 1
-}

arremessoBomba :: Jogador -> Tabuleiro -> Tabuleiro
arremessoBomba (_, (x, y), (_, _, (Arremesso, z)), a) t
	| z > 0 && a == N && pegaCoordenada t (x - 1, y) == Pilha7 = pegaCoordenada t (x - z, y) = Pilha7 && pegaCoordenada t (x - 1, y) = Pilha2 && z = 0
	| z > 0 && a == S && pegaCoordenada t (x + 1, y) == Pilha7 = pegaCoordenada t (x + z, y) = Pilha7 && pegaCoordenada t (x + 1, y) = Pilha2 && z = 0
	| z > 0 && a == L && pegaCoordenada t (x, y - 1) == Pilha7 = pegaCoordenada t (x, y + z) = Pilha7 && pegaCoordenada t (x, y - 1) = Pilha2 && z = 0
	| z > 0 && a == O && pegaCoordenada t (x, y + 1) == Pilha7 = pegaCoordenada t (x, y - z) = Pilha7 && pegaCoordenada t (x, y + 1) = Pilha2 && z = 0
	| otherwise = error "Não é possível arremessar"

-- recebe um jogador e o tabuleiro, verifica se nas coordenados do jogador no tabuleiro tem um presente
-- se tiver um presente tira o presente da Célula(pop na Pilha) e soma 1 na capacidade 
-- do jogador correspondente ao presente
-- retorna um novo jogador atualizado e um novo tabuleiro atualizado
coletaPresente :: Jogador -> Presente -> Jogador
coletaPresente (_, _, ((Patins, x), _, (Arremesso, y)), _) p
		if p == Patins then (_, _, ((Patins, x + 1), _, (Arremesso, y)), _)
		else if p == Arremesso then (_, _, ((Patins, x), _, (Arremesso, y + 1)), _)

explosão = undefined

fimDeJogo :: [Jogador] -> Bool
fimDeJogo = if (length (listaJogadores)) == 1




                                              
--------------------------------------------------------------------------------------------------------- 


{-
linha1 :: Linha
linha1 = ([Grama, Jogador1 ], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama, Jogador 2] )
linha2 :: Linha
linha2 = ([Grama, Parede], [Grama], [Grama], [Grama], [Pedra], [Grama], [Grama], [Grama])
linha3 :: Linha
linha3 = ([Grama], [Grama], [Grama, Parede], [Grama], [Grama], [Grama], [Grama], [Grama])
linha4 :: Linha
linha4 = ([Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama])
linha5 :: Linha
linha5 = ([Grama], [Grama], [Pedra], [Grama, Parede], [Grama], [Grama], [Grama], [Grama])
linha6 :: Linha
linha6 = ([Grama], [Grama], [Grama], [Grama], [Grama], [Grama, Parede], [Grama], [Grama])
linha7 :: Linha
linha7 = ([Grama], [Grama], [Grama], [Grama, Parede], [Grama], [Grama], [Grama], [Grama])
linha8 :: Linha
linha8 = ([Grama, Jogador3 ], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama, Jogador4 ])
-}

--------------------------------------------------------------------------------------------------------- 

--cada célula é uma pilha e cada pilha é uma lista de Itens
--Célula = Pilha = [item]
       
-------------------------------------------------------------------------------------------------------
-- função que remove um jogador da lista de jogadores
removeJogador :: Jogador -> [Jogador] -> [Jogador]
removeJogador _ [] = []
removeJogador x (y:ys) | x == y = removeJogador x ys
                       | otherwise = y : removeJogador x ys
	        

---------------------------------------------------------------------------------------------------------

--N', 'S', 'L' e 'O'
--grama
--presente_patins
--presente_arremesso
--bomba
--jogador_X



		