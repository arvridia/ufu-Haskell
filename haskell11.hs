-- Alunos:
-- Arthur Resende Santos 
-- Henrique Braga Alves Pereira
--Trabalho no momento esta incompleto, pois tivemos dificuldades principalmente na modificacao do tabuleiro
--e no casamento das funcoes.

type id = Int
type localizacao = (Int, Int)
type dir = N | S | L | O
data bloco = grama | parede | pedra | bomba
data item = presente_patins | presente_arremesso
type capacidades = ((presente_patins,Int), (bomba,Int), (presente_arremesso,Int))
type jogador = (id,localizacao,dir,capacidades)
type celula = [[a],[b],[c],[d]]
--type celula = ([grama||presente||parede||pedra||bomba||jogador],[grama||presente||parede||pedra||bomba||jogador],[grama||presente||parede||pedra||bomba||jogador],[grama||presente||parede||pedra||bomba||jogador])
type linha = [celula,celula,celula,celula,celula,celula,celula,celula]
type tabuleiro = [linha,linha,linha,linha,linha,linha,linha,linha]


-- esta funcao verifica se a celula criada eh valida, ou seja, nao possui uma combinacao de itens invalida
celula_valida :: celula -> Bool
celula_valida [[a],[b],[c],[d]]
 | x == [[],[],[],[]] = True
 | a /= (grama || pedra || parede) = False
 | b == (presente_patins || presente_arremesso || bomba) && (a /= grama) = False
 | b || c || d == pedra = False
 | (b || c || d == parede) && a /= grama = False
 | (b || c || d == jogador) && a /= grama = False
 | otherwise = True
 
--verifica se a linha possui todas as celulas validas
linha_valida :: linha -> Bool
linha_valida [celula1,celula2,celula3,celula4,celula5,celula6,celula7,celula8] = celula_valida celula1 == True && celula_valida celula2 == True && celula_valida celula3 == True && celula_valida celula4 == True && celula_valida celula5 == True && celula_valida celula6 == True && celula_valida celula7 == True && celula_valida celula8 == True

--ao receber as celulas do tabuleiro como parametro, verifica se todas sao validas
cria_tabuleiro :: [a] -> tabuleiro
cria_tabuleiro x@[linha1,linha2,linha3,linha4,linha5,linha6,linha7,linha8] = if (linha_valida linha1 == True && celula_valida linha2 == True && linha_valida linha3 == True && linha_valida linha4 == True && linha_valida linha5 == True && linha_valida linha6 == True && linha_valida linha7 == True && linha_valida linha8 == True) then x
	

--verifica se na celula ha algum jogador
existe_jogador :: celula -> Bool
existe_jogador [] = False
existe_jogador (x:xs) 
  | x == jogador = True
  | otherwise = existe_jogador xs

-- em implementacao
-- A função recebe como parametro o tabuleiro e os jogadores inseridos no tabuleiro e retorna um novo tabuleiro modificado.
-- A chamada da função verifica se o poder de arremesso esta disponivel para o jogador, de modo que se estiver ele ira utilizar
-- e de acordo com o numero de arremessos disponiveis ele ira lançar a bomba esse mesmo numero de casas.
arremesso :: tabuleiro -> jogador -> tabuleiro
arremesso x (a,b,c,(,,(y,w)))
        | w == 0 = x
        | otherwise = novotabuleiro
        where novotabuleiro =

--alternativa para a funcao existe_jogador
existe_jogador :: celula -> Bool
existe_jogador [[a],[b],[c],[d]] = a == (_,_,_,_) || b == (_,_,_,_) || c == (_,_,_,_) || d == (_,_,_,_)

--recebe o jogador de uma determinada celula
pega_jogador :: celula -> jogador
pega_jogador [[a],[b],[c],[d]]
 | existe_jogador a == True = a
 | existe_jogador b == True = b
 | existe_jogador c == True = c
 | otherwise = d

--em implementacao
--verifica o tabuleiro em busca dos jogadores a fim de montar uma lista com eles
lista_jogadores :: tabuleiro -> [Jogador]
lista_jogadores [] = []
lista_jogadores (x:xs) l
 | existe_jogador (head x) = head x:l
 | existe_jogador (head xs) = head xs:l
 | otherwise = lista_jogadores xs

--esta funcao recebe um tabuleiro e retorna outro com a modificacao do movimento do personagem ja efetuada, mas antes verifica se a celula esta disponivel para ser ocupada
movimenta :: tabuleiro -> jogador -> localizacao -> tabuleiro
movimenta t j (x,y) = if celula_disponivel (x,y) == True then modifica t j (x,y)
                                                         else error "este movimento nao eh possivel"


--caso a lista dos jogadores so possua um elemento, este jogador eh o vencedor e o jogo acabou
jogo_acabou :: tabuleiro -> String -> Bool
jogo_acabou x l = if l null then tail(lista_jogadores x) == [] else error "insira uma lista vazia"