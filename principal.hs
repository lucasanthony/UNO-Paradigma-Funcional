module Principal where

import System.Process
import DeckFunctions
import CartaFunctions
import JogadorFunctions

type Vez = Int

pilha = [(5,"AZUL","none"),(-1,"VERMELHA","+2"),(1,"VERMELHA","none"),(2,"VERDE","none"),(7,"AMARELA","none"),(9,"VERDE","none"),(2,"AMARELA","none"),(1,"VERMELHA","none"),(8,"VERDE","none"),(00,"VERDE","block"),(4,"AZUL","none")]
deck1 = [(1,"AZUL","none"),(-1,"AZUL","+2"),(1,"VERMELHA","none"),(2,"VERDE","none"),(1,"AMARELA","none"),(7,"AMARELA","reverse")]
deck2 = [(5,"VERDE","none"),(6,"VERMELHA","none"),(0,"VERMELHA","none"),(7,"VERMELHA","none"),(0,"AZUL","none"),(0,"VERMELHA","reverse")]
deck3 = [(7,"AMARELA","none"),(3,"AMARELA","none"),(3,"AZUL","none"),(5,"VERMELHA","none"),(1,"VERDE","none"),(10,"VERDE","reverse")]
deck4 = [(9,"AZUL","none"),(4,"AZUL","none"),(1,"VERDE","none"),(8,"AMARELA","none"),(2,"VERMELHA","none")]

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
  putStrLn ("\nAté breve \n")
executarOpcao '2' = do
  showRules
  getChar
  menu
executarOpcao _ = do
        putStrLn ("\nOpção inválida! Tente novamente...")
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
 putStr ("--------------------------------------------------------------------\n")
 if (venceu deck1) then do
     putStrLn ("Você venceu, parabéns!!")
 else if (venceu deck2) then do
     putStrLn ("Lula agora está livre, você perdeu!!")
 else if (venceu deck3) then do
     putStrLn ("Dilmãe voltou à presidência, você perdeu!!")
 else if (vez == 1) then do
    showTopo topo
    gerenciaPlayer topo pilha jogador1 deck1 deck2 deck3 reversed
 else if (vez == 2) then do
     showTopo topo
     gerenciaBot1 topo pilha jogador1 deck1 deck2 deck3 reversed
 else do
   showTopo topo
   gerenciaBot2 topo pilha jogador1 deck1 deck2 deck3 reversed

gerenciaPlayer :: Carta -> Deck -> Nome -> Deck -> Deck -> Deck -> Bool -> IO ()
gerenciaPlayer topo pilha jogador1 deck1 deck2 deck3 reversed = do
  if (podeJogar deck1 topo) then do -- SE TEM CARTA QUE DA MATCH
    putStrLn ("  Sua vez - " ++ (statusRodada reversed))
    status deck2 deck3
    showCards deck1 0
    putStrLn ("\nEscolha uma carta: ")
    opcao <- getLine
    let op = read opcao
    if (op >= 0 && op < size deck1 && cartaValida (getCarta deck1 op) topo) then do -- SE A CARTA FOR VÁLIDA
      if (reversed == True) then do -- SE JOGO ESTÁ INVERTIDO
          if (getEffect(getCarta deck1 op) == "reverse") then do -- carta reverse
            rodarJogo (getCarta deck1 op) pilha jogador1 (pickPlay deck1 op) deck2 deck3 2 False
          else if (getEffect(getCarta deck1 op) == "block") then do -- carta block
            msgBlock 1 reversed
            rodarJogo (getCarta deck1 op) pilha jogador1 (pickPlay deck1 op) deck2 deck3 2 reversed
          else if (getEffect(getCarta deck1 op) == "+2") then do -- carta +2
            rodarJogo (getCarta deck1 op) (tiraDuas pilha) jogador1 (pickPlay deck1 op) deck2 (deck3++(pegaDuas pilha)) 3 reversed
          else do rodarJogo (getCarta deck1 op) pilha jogador1 (pickPlay deck1 op) deck2 deck3 3 reversed -- carta simples
      else do -- SE JOGO NÃO ESTÁ INVERTIDO
          if (getEffect(getCarta deck1 op) == "reverse") then do
            rodarJogo (getCarta deck1 op) pilha jogador1 (pickPlay deck1 op) deck2 deck3 3 True
          else if (getEffect(getCarta deck1 op) == "block") then do -- carta block
            msgBlock 1 reversed
            rodarJogo (getCarta deck1 op) pilha jogador1 (pickPlay deck1 op) deck2 deck3 3 reversed
          else if (getEffect(getCarta deck1 op) == "+2") then do -- carta +2
            rodarJogo (getCarta deck1 op) (tiraDuas pilha) jogador1 (pickPlay deck1 op) (deck2++(pegaDuas pilha)) deck3 2 reversed
          else do rodarJogo (getCarta deck1 op) pilha jogador1 (pickPlay deck1 op) deck2 deck3 2 reversed -- carta simples
    else do
      putStrLn ("Carta inválida, tente outra carta!!\n")
      rodarJogo topo pilha jogador1 deck1 deck2 deck3 1 reversed
  else do -- SE NAO TEM CARTA QUE DA MATCH
    putStrLn "\nVoce nao possui carta valida, pegue uma da pilha pressionando <Enter>"
    getChar
    if (cartaValida (pegaUma pilha) topo) then do
         putStrLn (showCard (pegaUma pilha) ++ "jogada\n")
         if (reversed == True) then do
             if (getEffect(pegaUma pilha) == "block") then do
               msgBlock 1 reversed
               rodarJogo (pegaUma pilha) (tiraUma pilha) jogador1 deck1 deck2 deck3 2 reversed
             else if (getEffect(pegaUma pilha) == "reverse") then do
               rodarJogo (pegaUma pilha) (tiraUma pilha) jogador1 deck1 deck2 deck3 2 False
             else if (getEffect(pegaUma pilha) == "+2") then do
               let pilla = tiraUma pilha
               rodarJogo (pegaUma pilha) (tiraDuas pilla) jogador1 deck1 deck2 (deck3++(pegaDuas pilla)) 3 reversed
             else do rodarJogo (pegaUma pilha) (tiraUma pilha) jogador1 deck1 deck2 deck3 3 reversed
         else do
           if (getEffect(pegaUma pilha) == "block") then do
             msgBlock 1 reversed
             rodarJogo (pegaUma pilha) (tiraUma pilha) jogador1 deck1 deck2 deck3 3 reversed
           else if (getEffect(pegaUma pilha) == "reverse") then do
             rodarJogo (pegaUma pilha) (tiraUma pilha) jogador1 deck1 deck2 deck3 3 True
           else if (getEffect(pegaUma pilha) == "+2") then do
             let pilla = tiraUma pilha
             rodarJogo (pegaUma pilha) (tiraDuas pilla) jogador1 deck1 (deck2++(pegaDuas pilla)) deck3 2 reversed
           else do rodarJogo (pegaUma pilha) (tiraUma pilha) jogador1 deck1 deck2 deck3 2 reversed
    else do
      putStrLn (showCard (pegaUma pilha) ++ "adicionada em sua mão\n")
      if (reversed == True) then do
        rodarJogo topo (tiraUma pilha) jogador1 (deck1 ++ [pegaUma pilha]) deck2 deck3 3 reversed
      else do rodarJogo topo (tiraUma pilha) jogador1 (deck1 ++ [pegaUma pilha]) deck2 deck3 2 reversed

gerenciaBot1 :: Carta -> Deck -> Nome -> Deck -> Deck -> Deck -> Bool -> IO ()
gerenciaBot1 topo pilha jogador1 deck1 deck2 deck3 reversed = do
        if (podeJogar deck2 topo) then do
          putStrLn ("     Vez de Lula\n")
          showCards deck2 0
          putStrLn ("\nEscolha uma carta: ")
          opcao <- getLine
          let op = read opcao
          if (op >= 0 && op < size deck2 && cartaValida (getCarta deck2 op) topo) then do
            if (reversed == True) then do
              if (getEffect(getCarta deck2 op) == "reverse") then do
                rodarJogo (getCarta deck2 op) pilha jogador1 deck1 (pickPlay deck2 op) deck3 3 False
              else if (getEffect(getCarta deck2 op) == "block") then do
                msgBlock 2 reversed
                rodarJogo (getCarta deck2 op) pilha jogador1 deck1 (pickPlay deck2 op) deck3 3 reversed
              else if (getEffect(getCarta deck2 op) == "+2") then do
                rodarJogo (getCarta deck2 op) (tiraDuas pilha) jogador1 (deck1++(pegaDuas pilha)) (pickPlay deck2 op) deck3 1 reversed
              else do rodarJogo (getCarta deck2 op) pilha jogador1 deck1 (pickPlay deck2 op) deck3 1 reversed
            else do
              if (getEffect(getCarta deck2 op) == "reverse") then do
                rodarJogo (getCarta deck2 op) pilha jogador1 deck1 (pickPlay deck2 op) deck3 1 True
              else if (getEffect(getCarta deck2 op) == "block") then do
                msgBlock 2 reversed
                rodarJogo (getCarta deck2 op) pilha jogador1 deck1 (pickPlay deck2 op) deck3 1 reversed
              else if (getEffect(getCarta deck2 op) == "+2") then do
                rodarJogo (getCarta deck2 op) (tiraDuas pilha) jogador1 deck1 (pickPlay deck2 op) (deck3++(pegaDuas pilha)) 3 reversed
              else do rodarJogo (getCarta deck2 op) pilha jogador1 deck1 (pickPlay deck2 op) deck3 3 reversed
          else do
             putStrLn "\nTente outra carta!!"
             rodarJogo topo pilha jogador1 deck1 deck2 deck3 2 reversed
        else do
          putStrLn "\nVoce nao possui carta valida, pegue uma da pilha pressionando <Enter>"
          getChar
          if (cartaValida (pegaUma pilha) topo) then do
               putStrLn (showCard (pegaUma pilha) ++ "jogada\n")
               if (reversed == True)
                 then do rodarJogo (pegaUma pilha) (tiraUma pilha) jogador1 deck1 deck2 deck3 1 reversed
               else do rodarJogo (pegaUma pilha) (tiraUma pilha) jogador1 deck1 deck2 deck3 3 reversed
          else do
            putStrLn (showCard (pegaUma pilha) ++ "adicionada em sua mão\n")
            if (reversed == True) then do
              rodarJogo topo (tiraUma pilha) jogador1 deck1 (deck2 ++ [pegaUma pilha]) deck3 1 reversed
            else do rodarJogo topo (tiraUma pilha) jogador1 deck1 (deck2 ++ [pegaUma pilha]) deck3 3 reversed

gerenciaBot2 :: Carta -> Deck -> Nome -> Deck -> Deck -> Deck -> Bool -> IO ()
gerenciaBot2 topo pilha jogador1 deck1 deck2 deck3 reversed = do
        putStrLn ("     Vez de Dilmãe\n")
        showCards deck3 0
        putStrLn ("\nEscolha uma carta: ")
        opcao <- getLine
        let op = read opcao
        if (podeJogar deck3 topo) then do
          if (op >= 0 && op < size deck3 && cartaValida (getCarta deck3 op) topo) then do
            if (getEffect(getCarta deck3 op) == "reverse" && reversed == False) then do
              rodarJogo (getCarta deck3 op) pilha jogador1 deck1 deck2 (pickPlay deck3 op) 2 True
            else if (getEffect(getCarta deck3 op) == "reverse" && reversed == True) then do
              rodarJogo (getCarta deck3 op) pilha jogador1 deck1 deck2 (pickPlay deck3 op) 1 False
            else if (getEffect(getCarta deck3 op) == "block") then do
              msgBlock 3 reversed
              rodarJogo (getCarta deck3 op) pilha jogador1 deck1 deck2 (pickPlay deck3 op) 2 reversed
            else if (getEffect(getCarta deck3 op) == "+2") then do
              if (reversed == True) then do
                rodarJogo (getCarta deck3 op) (tiraDuas pilha) jogador1 deck1 (deck2++(pegaDuas pilha)) (pickPlay deck3 op) 2 reversed
              else do rodarJogo (getCarta deck3 op) (tiraDuas pilha) jogador1 (deck1++(pegaDuas pilha)) deck2 (pickPlay deck3 op) 1 reversed
            else if (reversed == True) then do
              rodarJogo (getCarta deck3 op) pilha jogador1 deck1 deck2 (pickPlay deck3 op) 2 reversed
            else do rodarJogo (getCarta deck3 op) pilha jogador1 deck1 deck2 (pickPlay deck3 op) 1 reversed
          else do
             putStrLn "\nTente outra carta!!"
             rodarJogo topo pilha jogador1 deck1 deck2 deck3 3 reversed
        else do
          putStrLn "\nVoce nao possui carta valida, pegue uma da pilha pressionando <Enter>"
          getChar
          if (cartaValida (pegaUma pilha) topo) then do
               putStrLn (showCard (pegaUma pilha) ++ "jogada\n")
               if (reversed == True) then do
                 rodarJogo (pegaUma pilha) (tiraUma pilha) jogador1 deck1 deck2 deck3 2 reversed
               else do rodarJogo (pegaUma pilha) (tiraUma pilha) jogador1 deck1 deck2 deck3 1 reversed
          else do
            putStrLn (showCard (pegaUma pilha) ++ "adicionada em sua mão\n")
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
