module Principal where

import Control.Exception
import System.IO
import System.IO.Error
import System.Process
import DeckFunctions
import CartaFunctions
import JogadorFunctions

type Vez = Int

pilha = [(5,"blue","none"),(-1,"red","+2"),(1,"red","none"),(2,"green","none"),(7,"yellow","none")]
deck1 = [(1,"blue","none"),(-1,"blue","+2"),(1,"red","none"),(2,"green","none"),(1,"yellow","none"),(7,"yellow","reverse")]
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
      	novoJogo dados jogador1 deck1 deck2 deck3 deck4

novoJogo :: Jogadores -> Nome -> Deck -> Deck -> Deck -> Deck -> IO Jogadores
novoJogo dados jogador1 deck1 deck2 deck3 deck4 = do
          putStrLn ("\nIniciando o jogo \"" ++
              jogador1 ++ " vs Lula vs Dilma vs Temer" ++ "\" ... ")
          putStrLn ("lets do this!!\n")
          rodarJogo dados (0,"first card","none") pilha jogador1 deck1 deck2 deck3 deck4 1 False



rodarJogo :: Jogadores -> Carta -> Deck -> Nome -> Deck -> Deck -> Deck -> Deck-> Vez -> Bool -> IO Jogadores
rodarJogo dados topo pilha jogador1 deck1 deck2 deck3 deck4 vez reversed = do
 cleanScreen
 showTopo topo
 if (vez == 1)
   then do
		 showCards deck1 0
		 op <- getLine
		 if ((getColor topo == getColor ((getCarta deck1 (read op))) || getColor(topo) == "first card") || getNumber topo == getNumber((getCarta deck1(read op))))
			 then do
				 putStrLn "\nBoa jogada!"
				 -- SE FOR A CARTA 'REVERSE' E O JOGO TIVER NO CURSO NORMAL, CHAMA O JOGADOR ANTERIOR E REVERSED TRUE
				 if (getEffect(getCarta deck1 (read op)) == "reverse" && reversed == False)
					 then do
						 rodarJogo dados (getCarta deck1 (read op)) pilha jogador1 (pickPlay deck1 (read op)) deck2 deck3 deck4 4 True
				 -- SE FOR A CARTA 'REVERSE' E O JOGO TIVER NO CURSO INVERSO, CHAMA O PROXIMO JOGADOR E REVERSED FALSE
				 else if (getEffect(getCarta deck1 (read op)) == "reverse" && reversed == True)
					 then do
						 rodarJogo dados (getCarta deck1 (read op)) pilha jogador1 (pickPlay deck1 (read op)) deck2 deck3 deck4 2 False
			   -- SE FOR A CARTA BLOCK, CHAMA O JOGADOR '3'
				 else if (getEffect(getCarta deck1 (read op)) == "block")
				   then do
				  	 rodarJogo dados (getCarta deck1 (read op)) pilha jogador1 (pickPlay deck1 (read op)) deck2 deck3 deck4 3 reversed
			   -- SE FOR A CARTA +2
				 else if (getEffect(getCarta deck1 (read op)) == "+2")
				   then do
			       -- SE TIVER INVERTIDO, TIRA 2 DA PILHA E COLOCA NA MAO DO 4
					   if (reversed == True)
					  	 then do
					  		 rodarJogo dados (getCarta deck1 (read op)) (tiraDuas pilha) jogador1 (pickPlay deck1 (read op)) deck2 deck3 (deck4++(pegaDuas pilha)) 4 reversed
				  	 -- SE TIVER NORMAL, TIRA 2 DA PILHA E COLOCA NA MAO DO 2
						 else do
							 rodarJogo dados (getCarta deck1 (read op)) (tiraDuas pilha) jogador1 (pickPlay deck1 (read op)) (deck2++(pegaDuas pilha)) deck3 deck4 2 reversed
			   -- SE FOR CARTA NORMAL E O JOGO TIVER INVERTIDO, CHAMA O JOGADOR 4
				 else if (reversed == True)
		  		 then do
			  		 rodarJogo dados (getCarta deck1 (read op)) pilha jogador1 (pickPlay deck1 (read op)) deck2 deck3 deck4 4 reversed
			   -- SE CHEGOU AQUI, ESTA TUDO COMO INICIA, CHAMA COMO ESTA
				 else do
				   rodarJogo dados (getCarta deck1 (read op)) pilha jogador1 (pickPlay deck1 (read op)) deck2 deck3 deck4 2 reversed
		 else do
			 putStrLn "\nTente outra carta!!"
			 rodarJogo dados topo pilha jogador1 deck1 deck2 deck3 deck4 1 reversed

          -- OS DEMAIS IRAO JOGAR AUTOATICAMENTE (BOTS)
 else if (vez == 2)
   then do putStr ("Lula, é a sua vez! \n")
           putStrLn ("sua mao :\n" ++ show(showDeck deck2))
           op <- getLine
           if (((getColor topo) == getColor (getCarta deck2 (read op)) || (getNumber topo) == getNumber (getCarta deck2 (read op))) && (read op >= 0 && read op < size deck2))
             then do putStrLn "\nBoa jogada"
                     if (getEffect(getCarta deck2 (read op)) == "reverse" && reversed == False)
                       then do
												 rodarJogo dados (getCarta deck2 (read op)) pilha jogador1 deck1 (pickPlay deck2 (read op)) deck3 deck4 1 True
                     else if (getEffect(getCarta deck2 (read op)) == "reverse" && reversed == True)
											 then do
												 rodarJogo dados (getCarta deck2 (read op)) pilha jogador1 deck1 (pickPlay deck2 (read op)) deck3 deck4 3 False
                     else if (reversed == True)
											 then do
												 rodarJogo dados (getCarta deck2 (read op)) pilha jogador1 deck1 (pickPlay deck2 (read op)) deck3 deck4 1 reversed
                     else do
											 rodarJogo dados (getCarta deck2 (read op)) pilha jogador1 deck1 (pickPlay deck2 (read op)) deck3 deck4 3 reversed
           else do putStrLn "\nTente outra carta!!"
                   rodarJogo dados topo pilha jogador1 deck1 deck2 deck3 deck4 2 reversed

 else if (vez == 3)
   then do putStr ("Dilma, é a sua vez! \n")
           putStrLn ("sua mao :\n" ++ show(showDeck deck3))
           op <- getLine
           if (((getColor topo) == getColor (getCarta deck3 (read op)) || (getNumber topo) == getNumber (getCarta deck3 (read op))) && (read op >= 0 && read op < size deck3))
		         then do
							 putStrLn "\nBoa jogada"
							 rodarJogo dados (getCarta deck3 (read op)) pilha jogador1 deck1 deck2 (pickPlay deck3 (read op)) deck4 4 reversed
           else do
						 putStrLn "\nTente outra carta!!"
						 rodarJogo dados topo pilha jogador1 deck1 deck2 deck3 deck4 3 reversed

 else do putStr ("Temer, é a sua vez! \n")
         putStrLn ("sua mao :\n" ++ show(showDeck deck4))
         op <- getLine
         if (((getColor topo) == getColor (getCarta deck4 (read op)) || (getNumber topo) == getNumber (getCarta deck4 (read op))) && (read op >= 0 && read op < size deck4))
				   then do
						 putStrLn "\nBoa jogada"
						 rodarJogo dados (getCarta deck4 (read op)) pilha jogador1 deck1 deck2 deck3 (pickPlay deck4 (read op)) 1 reversed
         else do
					 putStrLn "\nTente outra carta!!"
					 rodarJogo dados topo pilha jogador1 deck1 deck2 deck3 deck4 4 reversed

-- Funcão para mostrar o deck
showDeck :: Deck -> Deck
showDeck deck = deck

showLines :: [String] -> IO()
showLines [] = return()
showLines s = do
	putStrLn (head s)
	showLines (tail s)

-- Exibe a tela principal do game
tela_principal :: IO()
tela_principal = do
    cont <- readFile ".msg"
    showLines(take 37 (lines cont))

main :: IO()
main = do
    tela_principal

-- Limpa a tela
cleanScreen :: IO()
cleanScreen = putStr "\ESC[1J"

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
