module Principal where

import System.Process
import DeckFunctions
import CartaFunctions
import JogadorFunctions

type Vez = Int

pilha = [(5,"blue","none"),(-1,"red","+2"),(1,"red","none"),(2,"green","none"),(7,"yellow","none"),(9,"green","none"),(2,"yellow","none"),(1,"red","none"),(8,"green","none")]
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
  menu

menu :: IO ()
menu = do
    system "cls" -- limpa a tela (windows somente)
    tela_principal
    op <- getChar
    getChar
    executarOpcao op

executarOpcao :: Char -> IO ()
executarOpcao '1' = prepararJogo
executarOpcao '0' = do
  putStrLn ("\nAte breve \n")
  menu
executarOpcao '2' = do
  showRules
  getChar
  menu
executarOpcao _ = do
        putStrLn ("\nOpção inválida! Tente novamente...")
        putStr "\nPressione <Enter> para voltar ao menu..."
        getChar
        menu

prepararJogo :: IO ()
prepararJogo = do
      jogador1 <- getString "\nDigite seu login: "
      novoJogo jogador1 deck1 deck2 deck3

novoJogo :: Nome -> Deck -> Deck -> Deck -> IO ()
novoJogo jogador1 deck1 deck2 deck3 = do
          putStrLn ("\nIniciando o jogo \"" ++
              jogador1 ++ " vs Lula vs Dilma" ++ "\" ... ")
          putStrLn ("lets do this!!\n")
          rodarJogo (0,"first card","none") pilha jogador1 deck1 deck2 deck3 1 False

rodarJogo :: Carta -> Deck -> Nome -> Deck -> Deck -> Deck -> Vez -> Bool -> IO ()
rodarJogo topo pilha jogador1 deck1 deck2 deck3 vez reversed = do
 cleanScreen
 if (venceu deck1)
   then do
     putStrLn ("Você venceu, parabéns!!")
 else if (venceu deck2)
   then do
     putStrLn ("Lula agora está livre, você perdeu!!")
 else if (venceu deck3)
   then do
     putStrLn ("Dilmãe voltou à presidência, você perdeu!!")
 else if (vez == 1)
  then do
    showTopo topo
    gerenciaPlayer topo pilha jogador1 deck1 deck2 deck3 reversed
 -- OS DEMAIS IRAO JOGAR AUTOATICAMENTE (BOTS)
 else if (vez == 2)
   then do
     showTopo topo
     gerenciaBot1 topo pilha jogador1 deck1 deck2 deck3 reversed

 else do
   showTopo topo
   gerenciaBot2 topo pilha jogador1 deck1 deck2 deck3 reversed

gerenciaPlayer :: Carta -> Deck -> Nome -> Deck -> Deck -> Deck -> Bool -> IO ()
gerenciaPlayer topo pilha jogador1 deck1 deck2 deck3 reversed = do
  putStrLn ("     Sua vez\n")
  showCards deck1 0
  opcao <- getLine
  let op = read opcao
  if (podeJogar deck1 topo) then do
    if (cartaValida (getCarta deck1 op) topo) then do
      -- SE FOR A CARTA 'REVERSE' E O JOGO TIVER NO CURSO NORMAL, CHAMA O JOGADOR ANTERIOR E REVERSED TRUE
      if (getEffect(getCarta deck1 op) == "reverse" && reversed == False) then do
        rodarJogo (getCarta deck1 op) pilha jogador1 (pickPlay deck1 op) deck2 deck3 3 True
      -- SE FOR A CARTA 'REVERSE' E O JOGO TIVER NO CURSO INVERSO, CHAMA O PROXIMO JOGADOR E REVERSED FALSE
      else if (getEffect(getCarta deck1 op) == "reverse" && reversed == True) then do
        rodarJogo (getCarta deck1 op) pilha jogador1 (pickPlay deck1 op) deck2 deck3 2 False
      -- SE FOR A CARTA BLOCK, CHAMA O JOGADOR '3'
      else if (getEffect(getCarta deck1 op) == "block") then do
        rodarJogo (getCarta deck1 op) pilha jogador1 (pickPlay deck1 op) deck2 deck3 3 reversed
      -- SE FOR A CARTA +2
      else if (getEffect(getCarta deck1 op) == "+2") then do
        -- SE TIVER INVERTIDO, TIRA 2 DA PILHA E COLOCA NA MAO DO 4
        if (reversed == True)
          then do rodarJogo (getCarta deck1 op) (tiraDuas pilha) jogador1 (pickPlay deck1 op) deck2 (deck3++(pegaDuas pilha)) 3 reversed
          -- SE TIVER NORMAL, TIRA 2 DA PILHA E COLOCA NA MAO DO 2
          else do rodarJogo (getCarta deck1 op) (tiraDuas pilha) jogador1 (pickPlay deck1 op) (deck2++(pegaDuas pilha)) deck3 2 reversed
      -- SE FOR CARTA NORMAL E O JOGO TIVER INVERTIDO, CHAMA O JOGADOR 4
      else if (reversed == True)
        then do rodarJogo (getCarta deck1 op) pilha jogador1 (pickPlay deck1 op) deck2 deck3 3 reversed
      -- SE CHEGOU AQUI, ESTA TUDO COMO INICIA, CHAMA COMO ESTA
      else do rodarJogo (getCarta deck1 op) pilha jogador1 (pickPlay deck1 op) deck2 deck3 2 reversed
    else do
       putStrLn "\nTente outra carta!!"
       rodarJogo topo pilha jogador1 deck1 deck2 deck3 1 reversed
  else do
    putStrLn "\nVoce nao possui carta valida, pegue uma da pilha pressionando <Enter>"
    getChar
    if (cartaValida (pegaUma pilha) topo)
      then do
         if (reversed == True)
           then do rodarJogo (pegaUma pilha) (tiraUma pilha) jogador1 deck1 deck2 deck3 3 reversed
         else do rodarJogo (pegaUma pilha) (tiraUma pilha) jogador1 deck1 deck2 deck3 2 reversed
    else do
      if (reversed == True) then do
        rodarJogo topo (tiraUma pilha) jogador1 (deck1 ++ [pegaUma pilha]) deck2 deck3 3 reversed
      else do rodarJogo (pegaUma pilha) (tiraUma pilha) jogador1 deck1 deck2 deck3 2 reversed

gerenciaBot1 :: Carta -> Deck -> Nome -> Deck -> Deck -> Deck -> Bool -> IO ()
gerenciaBot1 topo pilha jogador1 deck1 deck2 deck3 reversed = do
        putStrLn ("     Vez de Lula\n")
        showCards deck2 0
        opcao <- getLine
        let op = read opcao
        if (podeJogar deck2 topo) then do
          if (cartaValida (getCarta deck2 op) topo) then do
             -- SE FOR A CARTA 'REVERSE' E O JOGO TIVER NO CURSO NORMAL, CHAMA O JOGADOR ANTERIOR E REVERSED TRUE
            if (getEffect(getCarta deck2 op) == "reverse" && reversed == False) then do
              rodarJogo (getCarta deck2 op) pilha jogador1 deck1 (pickPlay deck2 op) deck3 1 True
        -- SE FOR A CARTA 'REVERSE' E O JOGO TIVER NO CURSO INVERSO, CHAMA O PROXIMO JOGADOR E REVERSED FALSE
            else if (getEffect(getCarta deck2 op) == "reverse" && reversed == True) then do
              rodarJogo (getCarta deck2 op) pilha jogador1 deck1 (pickPlay deck2 op) deck3 3 False
        -- SE FOR A CARTA BLOCK, CHAMA O JOGADOR '3'
            else if (getEffect(getCarta deck2 op) == "block") then do
              rodarJogo (getCarta deck2 op) pilha jogador1 deck1 (pickPlay deck2 op) deck3 1 reversed
        -- SE FOR A CARTA +2
            else if (getEffect(getCarta deck2 op) == "+2") then do
            -- SE TIVER INVERTIDO, TIRA 2 DA PILHA E COLOCA NA MAO DO 4
              if (reversed == True)
                then do rodarJogo (getCarta deck2 op) (tiraDuas pilha) jogador1 (deck1++(pegaDuas pilha)) (pickPlay deck2 op) deck3 1 reversed
               -- SE TIVER NORMAL, TIRA 2 DA PILHA E COLOCA NA MAO DO 2
                else do rodarJogo (getCarta deck2 op) (tiraDuas pilha) jogador1 deck1 (pickPlay deck2 op) (deck3++(pegaDuas pilha)) 3 reversed
        -- SE FOR CARTA NORMAL E O JOGO TIVER INVERTIDO, CHAMA O JOGADOR 4
            else if (reversed == True)
              then do rodarJogo (getCarta deck2 op) pilha jogador1 deck1 (pickPlay deck2 op) deck3 1 reversed
        -- SE CHEGOU AQUI, ESTA TUDO COMO INICIA, CHAMA COMO ESTA
            else do rodarJogo (getCarta deck2 op) pilha jogador1 deck1 (pickPlay deck2 op) deck3 3 reversed
          else do
             putStrLn "\nTente outra carta!!"
             rodarJogo topo pilha jogador1 deck1 deck2 deck3 2 reversed
        else do
          putStrLn "\nVoce nao possui carta valida, pegue uma da pilha pressionando <Enter>"
          getChar
          if (cartaValida (pegaUma pilha) topo)
            then do
               if (reversed == True)
                 then do rodarJogo (pegaUma pilha) (tiraUma pilha) jogador1 deck1 deck2 deck3 1 reversed
               else do rodarJogo (pegaUma pilha) (tiraUma pilha) jogador1 deck1 deck2 deck3 3 reversed
          else do
            if (reversed == True) then do
              rodarJogo topo (tiraUma pilha) jogador1 deck1 (deck2 ++ [pegaUma pilha]) deck3 1 reversed
            else do rodarJogo topo (tiraUma pilha) jogador1 deck1 (deck2 ++ [pegaUma pilha]) deck3 3 reversed

gerenciaBot2 :: Carta -> Deck -> Nome -> Deck -> Deck -> Deck -> Bool -> IO ()
gerenciaBot2 topo pilha jogador1 deck1 deck2 deck3 reversed = do
        putStrLn ("     Vez de Dilmãe\n")
        showCards deck3 0
        opcao <- getLine
        let op = read opcao
        if (podeJogar deck3 topo) then do
          if (cartaValida (getCarta deck3 op) topo) then do
             -- SE FOR A CARTA 'REVERSE' E O JOGO TIVER NO CURSO NORMAL, CHAMA O JOGADOR ANTERIOR E REVERSED TRUE
            if (getEffect(getCarta deck3 op) == "reverse" && reversed == False) then do
              rodarJogo (getCarta deck3 op) pilha jogador1 deck1 deck2 (pickPlay deck3 op) 1 True
        -- SE FOR A CARTA 'REVERSE' E O JOGO TIVER NO CURSO INVERSO, CHAMA O PROXIMO JOGADOR E REVERSED FALSE
            else if (getEffect(getCarta deck3 op) == "reverse" && reversed == True) then do
              rodarJogo (getCarta deck2 op) pilha jogador1 deck1 deck2 (pickPlay deck3 op) 2 False
        -- SE FOR A CARTA BLOCK, CHAMA O JOGADOR '3'
            else if (getEffect(getCarta deck3 op) == "block") then do
              rodarJogo (getCarta deck2 op) pilha jogador1 deck1 deck2 (pickPlay deck3 op) 2 reversed
                    -- SE FOR A CARTA +2
            else if (getEffect(getCarta deck3 op) == "+2") then do
                        -- SE TIVER INVERTIDO, TIRA 2 DA PILHA E COLOCA NA MAO DO 4
              if (reversed == True)
                then do rodarJogo (getCarta deck3 op) (tiraDuas pilha) jogador1 deck1 (deck2++(pegaDuas pilha)) (pickPlay deck3 op) 2 reversed
                           -- SE TIVER NORMAL, TIRA 2 DA PILHA E COLOCA NA MAO DO 2
                else do rodarJogo (getCarta deck3 op) (tiraDuas pilha) jogador1 (deck1++(pegaDuas pilha)) deck2 (pickPlay deck3 op) 1 reversed
                    -- SE FOR CARTA NORMAL E O JOGO TIVER INVERTIDO, CHAMA O JOGADOR 4
            else if (reversed == True)
              then do rodarJogo (getCarta deck3 op) pilha jogador1 deck1 deck2 (pickPlay deck3 op) 2 reversed
                    -- SE CHEGOU AQUI, ESTA TUDO COMO INICIA, CHAMA COMO ESTA
            else do rodarJogo (getCarta deck3 op) pilha jogador1 deck1 deck2 (pickPlay deck3 op) 1 reversed
          else do
             putStrLn "\nTente outra carta!!"
             rodarJogo topo pilha jogador1 deck1 deck2 deck3 3 reversed
        else do
          putStrLn "\nVoce nao possui carta valida, pegue uma da pilha pressionando <Enter>"
          getChar
          if (cartaValida (pegaUma pilha) topo)
            then do
               if (reversed == True)
                 then do rodarJogo (pegaUma pilha) (tiraUma pilha) jogador1 deck1 deck2 deck3 2 reversed
               else do rodarJogo (pegaUma pilha) (tiraUma pilha) jogador1 deck1 deck2 deck3 1 reversed
          else do
            if (reversed == True) then do
              rodarJogo topo (tiraUma pilha) jogador1 deck1 deck2 (deck3 ++ [pegaUma pilha]) 2 reversed
            else do rodarJogo topo (tiraUma pilha) jogador1 deck1 deck2 (deck3 ++ [pegaUma pilha]) 1 reversed

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

showRules :: IO()
showRules = do
    cont <- readFile "regras.msg"
    showLines(take 18 (lines cont))

main :: IO()
main = do
    tela_principal

-- Limpa a tela
cleanScreen :: IO()
cleanScreen = putStr "\ESC[1J"
