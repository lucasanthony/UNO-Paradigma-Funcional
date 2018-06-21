import Control.Exception
import System.IO
import System.IO.Error
import System.Process
import Data.List
import Data.Function

type Jogadores = [Jogador]
type Nome = String
type Pontuacao = Int
type Vez = Int
type Mao = [Carta]
type Deck = [Carta]
type Numero = Int
type Cor = String
type Efeito = String
type Carta = (Numero, Cor, Efeito)
data Jogador = Jogador Nome Pontuacao Mao
					deriving (Show, Read)

deck1 = [(1,"blue","none"),(3,"blue","none"),(1,"red","none"),(2,"green","none"),(7,"yellow","none"),(7,"yellow","reverse"),(3,"red","none")]
deck2 = [(5,"green","none"),(6,"red","none"),(0,"red","none"),(7,"red","none"),(0,"blue","none"),(0,"red","reverse")]
deck3 = [(7,"yellow","none"),(3,"yellow","none"),(3,"blue","none"),(5,"red","none"),(1,"green","none"),(10,"green","reverse")]
deck4 = [(9,"blue","none"),(4,"blue","none"),(1,"green","none"),(8,"yellow","none"),(2,"red","none")]

getString :: String -> IO String
getString str = do
       putStr str
       res <- getLine
       return res

inicio :: IO ()
inicio = do
    {catch (ler_arquivo) tratar_erro;}
    where
       			-- tenta ler o arquivo
      ler_arquivo = do
      {
        arq <- openFile "dados.txt" ReadMode; -- abre o arquivo para leitura
       	dados <- hGetLine arq; -- ler o conteúdo do arquivo
       	hClose arq; -- fecha o arquivo
        menu (read dados); -- passa os dados para a função menu
       	return ()
      }
      tratar_erro erro = if isDoesNotExistError erro then do
      {
       				-- se o arquivo NÃO existir, então cria o arquivo
       	arq <- openFile "dados.txt" WriteMode; -- abre o arquivo para escrita
       	hPutStrLn arq "[]"; -- escreve uma lista vazia no arquivo
       	hClose arq; -- fecha o arquivo
       	menu []; -- passa uma lista vazia para o menu
       	return ()
      }
      else
       	ioError erro

menu :: Jogadores -> IO Jogadores
menu dados = do
    system "cls" -- limpa a tela (windows somente)
    tela_principal
    op <- getChar
    getChar
    executarOpcao dados op

executarOpcao :: Jogadores -> Char -> IO Jogadores
executarOpcao dados '1' = cadastrarJogador dados
executarOpcao dados '2' = prepararJogo dados
executarOpcao dados '3' = do
    		putStrLn "\nRanking dos jogadores:\n"
    		if (null dados) then do
    			putStrLn ("Não há jogadores cadastrados!")
    		else
    					-- a função ordenar ordena crescentemente pela pontuação
    			exibirRanking (reverse (ordenar dados))
    		putStr "\nPressione <Enter> para voltar ao menu..."
    		getChar
    		menu dados
executarOpcao dados '0' = do
    		putStrLn ("\nAte breve \n")
    		return dados
executarOpcao dados '4' = do
				putStrLn ("\nCada jogador inicia o game com 7 cartas...")
				putStr "\nPressione <Enter> para voltar ao menu..."
				getChar
				menu dados
executarOpcao dados _ = do
    		putStrLn ("\nOpção inválida! Tente novamente...")
    		putStr "\nPressione <Enter> para voltar ao menu..."
    		getChar
    		menu dados

cadastrarJogador :: Jogadores -> IO Jogadores
cadastrarJogador dados = do
        nome <- getString "\nDigite um nome de usuário: "
        let mao = []
        if (existeJogador dados nome) then do
        	putStrLn "\nEsse nome já existe, escolha outro."
        	putStr "\nPressione <Enter> para continuar..."
        	getChar
        	menu dados
        else do
        	arq <- openFile "dados.txt" WriteMode -- abre o arquivo para escrita
        	hPutStrLn arq (show ((Jogador nome 0 mao):dados))
        	hClose arq -- fecha o arquivo
        	putStrLn ("\nUsuário " ++ nome ++ " cadastrado com sucesso.")
        	putStr "\nPressione <Enter> para continuar..."
        	getChar
        	menu ((Jogador nome 0 mao):dados) -- retorna a nova lista para o menu

-- Verifica se o nome do jogador já está cadastrado
existeJogador :: Jogadores -> Nome -> Bool
existeJogador [] _ = False
existeJogador ((Jogador n p m):xs) nome
      | (n == nome) = True
      | otherwise = existeJogador xs nome

prepararJogo :: Jogadores -> IO Jogadores
prepararJogo dados = do
      jogador1 <- getString "\nDigite seu login: "
      			-- testa se o jogador1 existe
      if not (existeJogador dados jogador1) then do
      	putStrLn "\nLogin nao cadastrado!"
      	putStr "\nPressione <Enter> para continuar..."
      	getChar -- descarta o Enter
      	menu dados
      else do 					-- se chegou aqui, é porque os quatro jogadores existem
      	novoJogo dados jogador1 deck1 "Lula" deck2 "Dilma" deck3 "Temer" deck4

novoJogo :: Jogadores -> Nome -> Deck -> Nome -> Deck -> Nome -> Deck -> Nome -> Deck -> IO Jogadores
novoJogo dados jogador1 deck1 jogador2 deck2 jogador3 deck3 jogador4 deck4 = do
          putStrLn ("\nIniciando o jogo \"" ++
              jogador1 ++ " vs " ++ jogador2 ++ " vs " ++ jogador3 ++ " vs " ++ jogador4 ++ "\" ... ")
          putStrLn ("lets do this!!\n")

          rodarJogo dados (0,"first card","none") jogador1 deck1 jogador2 deck2 jogador3 deck3 jogador4 deck4 1 False



rodarJogo :: Jogadores -> Carta ->  Nome -> Deck -> Nome -> Deck -> Nome -> Deck -> Nome -> Deck-> Vez -> Bool -> IO Jogadores
rodarJogo dados topo jogador1 deck1 jogador2 deck2 jogador3 deck3 jogador4 deck4 vez reversed = do
          showTopo topo
          if (vez == 1)
            then do showCards deck1 0
                    op <- getLine
                    if ((getColor topo == getColor ((getCarta deck1 (read op))) || getColor(topo) == "first card") || getNumber topo == getNumber((getCarta deck1(read op))))
                        then do putStrLn "\nBoa jogada!"
												        -- SE FOR A CARTA 'REVERSE' E O JOGO TIVER NO CURSO NORMAL, CHAMA O JOGADOR ANTERIOR E REVERSED TRUE
                                if (getEffect(getCarta deck1 (read op)) == "reverse" && reversed == False)
                                   then do rodarJogo dados (getCarta deck1 (read op)) jogador1 (tiraUma deck1 (read op)) jogador2 deck2 jogador3 deck3 jogador4 deck4 4 True
																-- SE FOR A CARTA 'REVERSE' E O JOGO TIVER NO CURSO INVERSO, CHAMA O PROXIMO JOGADOR E REVERSED FALSE
                                else if (getEffect(getCarta deck1 (read op)) == "reverse" && reversed == True)
																	 then do rodarJogo dados (getCarta deck1 (read op)) jogador1 (tiraUma deck1 (read op)) jogador2 deck2 jogador3 deck3 jogador4 deck4 2 False
																-- SE FOR A CARTA BLOCK, CHAMA O JOGADOR '3'
																else if (getEffect(getCarta deck1 (read op)) == "block")
	 																 then do rodarJogo dados (getCarta deck1 (read op)) jogador1 (tiraUma deck1 (read op)) jogador2 deck2 jogador3 deck3 jogador4 deck4 3 reversed
																-- SE FOR CARTA NORMAL E O JOGO TIVER INVERTIDO, CHAMA O JOGADOR 4
                                else if (reversed == True)
																	 then do rodarJogo dados (getCarta deck1 (read op)) jogador1 (tiraUma deck1 (read op)) jogador2 deck2 jogador3 deck3 jogador4 deck4 4 reversed
																-- SE CHEGOU AQUI, ESTA TUDO COMO INICIA, CHAMA COMO ESTA
																else do
                                   rodarJogo dados (getCarta deck1 (read op)) jogador1 (tiraUma deck1 (read op)) jogador2 deck2 jogador3 deck3 jogador4 deck4 2 reversed
                    else do putStrLn "\nTente outra carta!!"
                            rodarJogo dados topo jogador1 deck1 jogador2 deck2 jogador3 deck3 jogador4 deck4 1 reversed

          -- OS DEMAIS IRAO JOGAR AUTOATICAMENTE (BOTS)
          else if (vez == 2)
                then do putStr (jogador2 ++ ", é a sua vez! \n")
                        putStrLn ("sua mao :\n" ++ show(showDeck deck2))
                        op <- getLine
                        if (((getColor topo) == getColor (getCarta deck2 (read op)) || (getNumber topo) == getNumber (getCarta deck2 (read op))) && (read op >= 0 && read op < size deck2))
                            then do putStrLn "\nBoa jogada"
                                    if (getEffect(getCarta deck2 (read op)) == "reverse" && reversed == False)
                                       then do rodarJogo dados (getCarta deck2 (read op)) jogador1 deck1 jogador2 (tiraUma deck2 (read op)) jogador3 deck3 jogador4 deck4 1 True
                                    else if (getEffect(getCarta deck2 (read op)) == "reverse" && reversed == True) then do
                                       rodarJogo dados (getCarta deck2 (read op)) jogador1 deck1 jogador2 (tiraUma deck2 (read op)) jogador3 deck3 jogador4 deck4 3 False
                                    else if (reversed == True) then do
                                       rodarJogo dados (getCarta deck2 (read op)) jogador1 deck1 jogador2 (tiraUma deck2 (read op)) jogador3 deck3 jogador4 deck4 1 reversed
                                    else do
                                       rodarJogo dados (getCarta deck2 (read op)) jogador1 deck1 jogador2 (tiraUma deck2 (read op)) jogador3 deck3 jogador4 deck4 3 reversed
                        else do putStrLn "\nTente outra carta!!"
                                rodarJogo dados topo jogador1 deck1 jogador2 deck2 jogador3 deck3 jogador4 deck4 2 reversed

          else if (vez == 3) then do
                 putStr (jogador3 ++ ", é a sua vez! \n")
                 putStrLn ("sua mao :\n" ++ show(showDeck deck3))
                 op <- getLine
                 if (((getColor topo) == getColor (getCarta deck3 (read op)) || (getNumber topo) == getNumber (getCarta deck3 (read op))) && (read op >= 0 && read op < size deck3)) then do
                     putStrLn "\nBoa jogada"
                     rodarJogo dados (getCarta deck3 (read op)) jogador1 deck1 jogador2 deck2 jogador3 (tiraUma deck3 (read op)) jogador4 deck4 4 reversed
                 else do putStrLn "\nTente outra carta!!"
                         rodarJogo dados topo jogador1 deck1 jogador2 deck2 jogador3 deck3 jogador4 deck4 3 reversed

          else do
                 putStr (jogador4 ++ ", é a sua vez! \n")
                 putStrLn ("sua mao :\n" ++ show(showDeck deck4))
                 op <- getLine
                 if (((getColor topo) == getColor (getCarta deck4 (read op)) || (getNumber topo) == getNumber (getCarta deck4 (read op))) && (read op >= 0 && read op < size deck4)) then do
                     putStrLn "\nBoa jogada"
                     rodarJogo dados (getCarta deck4 (read op)) jogador1 deck1 jogador2 deck2 jogador3 deck3 jogador4 (tiraUma deck4 (read op)) 1 reversed
                 else do putStrLn "\nTente outra carta!!"
                         rodarJogo dados topo jogador1 deck1 jogador2 deck2 jogador3 deck3 jogador4 deck4 4 reversed

-- Funcão para mostrar o deck
showDeck :: Deck -> Deck
showDeck deck = deck

-- Função que verifica se o player venceu a partida,
-- verificação feita pelo deck do mesmo
venceu :: Deck -> Bool
venceu d | size d == 0 = True
         | otherwise = False

atualizaPontuacao :: Jogadores -> String -> Jogadores
atualizaPontuacao ((Jogador nome pontuacao mao):xs) vencedor
         | (nome == vencedor) = [(Jogador nome (pontuacao + 1) mao)] ++ xs
         | otherwise = (Jogador nome pontuacao mao):(atualizaPontuacao xs vencedor)

-- exibir ranking dos jogadores
-- critério: da maior para a menor pontuação
exibirRanking :: Jogadores -> IO ()
exibirRanking [] = return ()
exibirRanking (x:xs) = do
      putStrLn ((obterNome x) ++ " possui " ++ (show (obterPontuacao x)) ++ " vitorias")
      exibirRanking xs

-- função que recebe um jogador e retorna o nome
obterNome :: Jogador -> Nome
obterNome (Jogador nome _ _) = nome

-- função que recebe um jogador e retorna a pontuação
obterPontuacao :: Jogador -> Pontuacao
obterPontuacao (Jogador _ pontuacao _) = pontuacao

-- função que define o critério de ordenação
ordenar :: Jogadores -> Jogadores
ordenar dados = sortBy (compare `on` obterPontuacao) dados

-- Retorna o tamanho do deck
size :: Deck -> Int
size [] = 0
size (x:xs) = 1 + size xs

-- Funcao que retira 2 cartas do deck principal para colocar na mao de algum player
-- quando uma carta +2 for usada
tiraDuas :: Deck -> Deck
tiraDuas [x,y] = []
tiraDuas [x] = []
tiraDuas (x:xs) = [x] ++ tiraDuas xs

-- Funcao que retira 4 cartas do deck principal para colocar na mao de algum player
-- quando uma carta +4 for usada
tiraQuatro :: Deck -> Deck
tiraQuatro [a,b,c,d] = []
tiraQuatro [a,b,c] = []
tiraQuatro [a,b] = []
tiraQuatro [a] = []
tiraQuatro [] = []
tiraQuatro (x:xs) = [x] ++ tiraQuatro xs

--Funcao que retorna 2 cartas do deck principal que irao pra mao do player
pegaDuas :: Deck -> Deck
pegaDuas [x,y] = [x,y]
pegaDuas (x:xs) = pegaDuas xs

--Funcao que retorna 4 cartas do deck principal que irao pra mao do player
pegaQuatro :: Deck -> Deck
pegaQuatro [a,b,c,d] = [a,b,c,d]
pegaQuatro (x:xs) = pegaQuatro xs

-- Funcao para retirar uma carta especifica da mao do player
tiraUma :: Deck -> Int -> Deck
tiraUma [] _ = []
tiraUma (_:xs) 0 = xs
tiraUma (x:xs) n | n == 0 = tiraUma xs (n+1)
                 | otherwise = [x] ++ tiraUma xs (n-1)

-- Retorna a cor da carta
getColor :: Carta -> Cor
getColor (_,cor,_) = cor

-- Retorna o efeito da carta
getEffect :: Carta -> Efeito
getEffect (_,_,efeito) = efeito

-- Retorna o número da carta
getNumber :: Carta -> Numero
getNumber (n,_,_) = n

-- Retorna uma carta específica do deck passado como parâmetro
getCarta :: Deck -> Int -> Carta
getCarta ((n,cor,efeito):xs) x | x == 0 = (n,cor,efeito)
                                     | otherwise = getCarta xs (x-1)

showTopo :: Carta -> IO()
showTopo s = do
	putStrLn ("Topo : " ++ "Numero: " ++ show(getNumber s) ++ " Cor: " ++ getColor s ++ " Efeito: " ++ getEffect s ++ "\n")

showCards :: Deck -> Int -> IO()
showCards [] _ = return()
showCards s n = do
	putStrLn ((show n) ++ " - " ++ "Numero: " ++ show(getNumber(head s)) ++ " Cor: " ++ getColor(head s) ++ " Efeito: " ++ getEffect(head s))
	showCards (tail s) (n+1)

showLines :: [String] -> IO()
showLines [] = return()
showLines s = do
	putStrLn (head s)
	showLines (tail s)

tela_principal :: IO()
tela_principal = do
    cont <- readFile ".msg"
    showLines(take 37 (lines cont))

main :: IO()
main = do
    tela_principal

{-		BLOCO DE CODIGO Q VAI SER USADO QUANDO ALGUEM VENCER O JOGO
      if (venceuJogador1 tabela) then do
			putStrLn ("Parábens " ++ jogador1 ++ "! Você venceu!!")

			-- abre o arquivo para escrita para atualizá-lo
			arq_escrita <- openFile "dados.txt" WriteMode
			hPutStrLn arq_escrita (show (atualizaPontuacao dados jogador1))
			hClose arq_escrita

			-- abre o arquivo para leitura
			arq_leitura <- openFile "dados.txt" ReadMode
			dados_atualizados <- hGetLine arq_leitura
			hClose arq_leitura

			putStr "\nPressione <Enter> para voltar ao menu..."
			getChar
			menu (read dados_atualizados) -}
