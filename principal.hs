 module Principal where

import Control.Concurrent
import System.Process
import DeckFunctions
import CartaFunctions
import JogadorFunctions
import Bot
import Util
import MethodShuffle
import System.IO.Unsafe

type Vez = Int

pilha = unsafePerformIO $ (shuffle [(5,"AZUL"," "),(-1,"VERMELHA","+2"),(1,"VERMELHA"," "),(2,"VERDE"," "),(7,"AMARELA"," "),(9,"VERDE"," "),(2,"AMARELA"," "),(1,"VERMELHA"," "),(8,"VERDE"," "),(00,"VERDE","BLOCK"),(4,"AZUL"," ")])
deck1 = unsafePerformIO $ (shuffle [(1,"AZUL"," "),(-1,"AZUL","+2"),(1,"VERMELHA"," "),(2,"VERDE"," "),(2,"VERDE", "+2"),(1,"AMARELA"," "),(7,"AMARELA","REVERSE"), (20, "PRETA", "+4"), (30, "PRETA", "newColor")])
deck2 = unsafePerformIO $ (shuffle [(5,"VERDE"," "),(6,"VERMELHA"," "),(0,"VERMELHA"," "),(30,"PRETA","+4")])
deck3 = unsafePerformIO $ (shuffle [(7,"AMARELA"," "), (30,"PRETA","+4")])
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
    system "cls"
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
      putStrLn "Pressione qualquer tecla para iniciar"
      getChar
      novoJogo deck1 deck2 deck3

novoJogo ::  Deck ->  Deck ->  Deck -> IO ()
novoJogo deck1 deck2 deck3 = do
          putStrLn ("\nIniciando o jogo: Você vs Lula vs Dilma" ++ "\" ... ")
          putStrLn ("lets do this!!\n")
          rodarJogo (0,"first card","none") pilha deck1 deck2 deck3 1 False

rodarJogo :: Carta ->  Deck -> Deck -> Deck ->  Deck -> Vez -> Bool -> IO ()
rodarJogo topo pilha deck1 deck2 deck3 vez reversed = do
 putStr ("--------------------------------------------------------------------\n")
 if (venceu deck1) then do
     putStrLn ("Você venceu, parabéns!!")
 else if (venceu deck2) then do
     putStrLn ("Lula agora está livre, você perdeu!!")
 else if (venceu deck3) then do
     putStrLn ("Dilmãe voltou à presidência, você perdeu!!")
 else if (vez == 1) then do
    showTopo topo
    gerenciaPlayer topo pilha deck1 deck2 deck3 reversed
 else if (vez == 2) then do
     showTopo topo
     gerenciaBot1 topo pilha deck1 deck2 deck3 reversed
 else do
   showTopo topo
   gerenciaBot2 topo pilha deck1 deck2 deck3 reversed

gerenciaPlayer :: Carta -> Deck -> Deck ->  Deck ->  Deck -> Bool -> IO ()
gerenciaPlayer topo pilha deck1 deck2 deck3 reversed = do
  if (podeJogar deck1 topo) then do -- SE TEM CARTA QUE DA MATCH
      putStrLn ("  Sua vez - " ++ (next reversed))
      status deck2 deck3
      showCards deck1 topo 0
      putStrLn ("\nEscolha uma carta: ")
      opcao <- getLine
      if (checkInput opcao) then do
        let op = read opcao
        if (op >= 0 && op < size deck1 && cartaValida (getCarta deck1 op) topo) then do -- SE A CARTA FOR VÁLIDA
          if (reversed == True) then do -- SE JOGO ESTÁ INVERTIDO
              if (getEffect(getCarta deck1 op) == "REVERSE") then do -- carta reverse
                rodarJogo (getCarta deck1 op) pilha (pickPlay deck1 op) deck2 deck3 2 False
              else if (getEffect(getCarta deck1 op) == "BLOCK") then do -- carta block
                msgBlock 1 reversed
                rodarJogo (getCarta deck1 op) pilha (pickPlay deck1 op) deck2 deck3 2 reversed
              else if (getEffect(getCarta deck1 op) == "+2") then do -- carta +2
                rodarJogo (getCarta deck1 op) (tiraDuas pilha) (pickPlay deck1 op) deck2 (deck3++(pegaDuas pilha)) 3 reversed
              else if (getEffect(getCarta deck1 op) == "+4") then do -- carta +4
                putStrLn("Selecione uma cor entre AZUL, VERDE, AMARELA ou VERMELHA")
                novaCor <- getLine
                if(novaCor == "AZUL" || novaCor == "VERDE" || novaCor == "AMARELA" || novaCor == "VERMELHA") then do
                    rodarJogo (setColor novaCor (getCarta deck1 op)) (tiraQuatro pilha) (pickPlay deck1 op) deck2 (deck3++(pegaQuatro pilha)) 3 reversed
                else do
                    putStrLn("Cor inválida! Escolha a carta novamente.")
                    gerenciaPlayer topo pilha deck1 deck2 deck3 reversed
                    
              else if (getEffect(getCarta deck1 op) == "newColor") then do -- carta nova Cor
                putStrLn("Selecione uma cor entre AZUL, VERDE, AMARELA ou VERMELHA")
                novaCor <- getLine
                if(novaCor == "AZUL" || novaCor == "VERDE" || novaCor == "AMARELA" || novaCor == "VERMELHA") then do
                    rodarJogo (setColor novaCor (getCarta deck1 op)) pilha (pickPlay deck1 op) deck2 deck3 3 reversed
                else do
                    putStrLn("Cor inválida! Escolha a carta novamente.")
                    gerenciaPlayer topo pilha deck1 deck2 deck3 reversed
              else do rodarJogo (getCarta deck1 op) pilha (pickPlay deck1 op) deck2 deck3 3 reversed -- carta simples
          else do -- SE JOGO NÃO ESTÁ INVERTIDO
              if (getEffect(getCarta deck1 op) == "REVERSE") then do
                rodarJogo (getCarta deck1 op) pilha (pickPlay deck1 op) deck2 deck3 3 True
              else if (getEffect(getCarta deck1 op) == "BLOCK") then do -- carta block
                msgBlock 1 reversed
                rodarJogo (getCarta deck1 op) pilha (pickPlay deck1 op) deck2 deck3 3 reversed
              else if (getEffect(getCarta deck1 op) == "+2") then do -- carta +2
                rodarJogo (getCarta deck1 op) (tiraDuas pilha) (pickPlay deck1 op) (deck2++(pegaDuas pilha)) deck3 2 reversed
              else if (getEffect(getCarta deck1 op) == "+4") then do -- carta +4
                putStrLn("Selecione uma cor entre AZUL, VERDE, AMARELA ou VERMELHA")
                novaCor <- getLine
                if(novaCor == "AZUL" || novaCor == "VERDE" || novaCor == "AMARELA" || novaCor == "VERMELHA") then do
                    rodarJogo (setColor novaCor (getCarta deck1 op)) (tiraQuatro pilha) (pickPlay deck1 op) (deck2++(pegaQuatro pilha)) deck3 2 reversed
                else do
                    putStrLn("Cor inválida! Escolha a carta novamente.")
                    gerenciaPlayer topo pilha deck1 deck2 deck3 reversed
                    
              else if (getEffect(getCarta deck1 op) == "newColor") then do -- carta nova Cor
                putStrLn("Selecione uma cor entre AZUL, VERDE, AMARELA ou VERMELHA")
                novaCor <- getLine
                if(novaCor == "AZUL" || novaCor == "VERDE" || novaCor == "AMARELA" || novaCor == "VERMELHA") then do
                    rodarJogo (setColor novaCor (getCarta deck1 op)) pilha (pickPlay deck1 op) deck2 deck3 2 reversed
                else do
                    putStrLn("Cor inválida! Escolha a carta novamente.")
                    gerenciaPlayer topo pilha deck1 deck2 deck3 reversed
            
              else do rodarJogo (getCarta deck1 op) pilha (pickPlay deck1 op) deck2 deck3 2 reversed -- carta simples
        else do
          putStrLn ("Carta inválida, tente outra carta!!\n")
          rodarJogo topo pilha deck1 deck2 deck3 1 reversed
      else do
        putStrLn("Entrada inválida!\n")
        gerenciaPlayer topo pilha deck1 deck2 deck3 reversed
  else do -- SE NAO TEM CARTA QUE DA MATCH
    putStrLn "\nVoce nao possui carta valida, pegue uma da pilha pressionando <Enter>"
    getChar
    if (cartaValida (pegaUma pilha) topo) then do
         putStrLn (showCard (pegaUma pilha) ++ "jogada\n")
         if (reversed == True) then do
             if (getEffect(pegaUma pilha) == "BLOCK") then do
               msgBlock 1 reversed
               rodarJogo (pegaUma pilha) (tiraUma pilha) deck1 deck2 deck3 2 reversed
             else if (getEffect(pegaUma pilha) == "REVERSE") then do
               rodarJogo (pegaUma pilha) (tiraUma pilha) deck1 deck2 deck3 2 False
             else if (getEffect(pegaUma pilha) == "+2") then do
               let pilla = tiraUma pilha
               rodarJogo (pegaUma pilha) (tiraDuas pilla) deck1 deck2 (deck3++(pegaDuas pilla)) 3 reversed
             else do rodarJogo (pegaUma pilha) (tiraUma pilha) deck1 deck2 deck3 3 reversed
         else do
           if (getEffect(pegaUma pilha) == "BLOCK") then do
             msgBlock 1 reversed
             rodarJogo (pegaUma pilha) (tiraUma pilha) deck1 deck2 deck3 3 reversed
           else if (getEffect(pegaUma pilha) == "REVERSE") then do
             rodarJogo (pegaUma pilha) (tiraUma pilha) deck1 deck2 deck3 3 True
           else if (getEffect(pegaUma pilha) == "+2") then do
             let pilla = tiraUma pilha
             rodarJogo (pegaUma pilha) (tiraDuas pilla) deck1 (deck2++(pegaDuas pilla)) deck3 2 reversed
           else do rodarJogo (pegaUma pilha) (tiraUma pilha) deck1 deck2 deck3 2 reversed
    else do
      putStrLn (showCard (pegaUma pilha) ++ "adicionada em sua mão\n")
      if (reversed == True) then do
        rodarJogo topo (tiraUma pilha) (deck1 ++ [pegaUma pilha]) deck2 deck3 3 reversed
      else do rodarJogo topo (tiraUma pilha) (deck1 ++ [pegaUma pilha]) deck2 deck3 2 reversed

gerenciaBot1 :: Carta ->  Deck ->  Deck ->  Deck ->  Deck -> Bool -> IO ()
gerenciaBot1 topo pilha deck1 deck2 deck3 reversed = do
        putStrLn ("     Lula está jogando...\n")
        threadDelay 1500000
        if (podeJogar deck2 topo) then do
          if (reversed == True) then do
            let op = escolheJogada deck2 deck1 topo
            if (getEffect(getCarta deck2 op) == "REVERSE") then do
              rodarJogo (getCarta deck2 op) pilha deck1 (pickPlay deck2 op) deck3 3 False
            else if (getEffect(getCarta deck2 op) == "BLOCK") then do
              msgBlock 2 reversed
              rodarJogo (getCarta deck2 op) pilha deck1 (pickPlay deck2 op) deck3 3 reversed
            else if (getEffect(getCarta deck2 op) == "+2") then do
              rodarJogo (getCarta deck2 op) (tiraDuas pilha) (deck1++(pegaDuas pilha)) (pickPlay deck2 op) deck3 1 reversed
            else if (getEffect(getCarta deck2 op) == "+4") then do -- carta +4
              let newColor = selecionaCor deck2
              rodarJogo (setColor newColor (getCarta deck2 op)) (tiraQuatro pilha) (deck1++(pegaQuatro pilha)) (pickPlay deck2 op) deck3 1 reversed
            else if (getEffect(getCarta deck2 op) == "newColor") then do -- carta nova Cor
              let newColor = selecionaCor deck2
              rodarJogo (setColor newColor (getCarta deck2 op)) pilha deck1 (pickPlay deck2 op) deck3 1 reversed
            else do rodarJogo (getCarta deck2 op) pilha deck1 (pickPlay deck2 op) deck3 1 reversed
          else do
            let op = escolheJogada deck2 deck3 topo
            if (getEffect(getCarta deck2 op) == "REVERSE") then do
              rodarJogo (getCarta deck2 op) pilha deck1 (pickPlay deck2 op) deck3 1 True
            else if (getEffect(getCarta deck2 op) == "BLOCK") then do
              msgBlock 2 reversed
              rodarJogo (getCarta deck2 op) pilha deck1 (pickPlay deck2 op) deck3 1 reversed
            else if (getEffect(getCarta deck2 op) == "+2") then do
              rodarJogo (getCarta deck2 op) (tiraDuas pilha) deck1 (pickPlay deck2 op) (deck3++(pegaDuas pilha)) 3 reversed
            else if (getEffect(getCarta deck2 op) == "+4") then do -- carta +4
                let newColor = selecionaCor deck2
                rodarJogo (setColor newColor (getCarta deck2 op)) (tiraQuatro pilha) deck1 (pickPlay deck2 op) (deck3++(pegaQuatro pilha)) 3 reversed
                
            else if (getEffect(getCarta deck2 op) == "newColor") then do -- carta nova Cor
                let newColor = selecionaCor deck2
                rodarJogo (setColor newColor (getCarta deck2 op)) pilha deck1 (pickPlay deck2 op) deck3 3 reversed
            else do rodarJogo (getCarta deck2 op) pilha deck1 (pickPlay deck2 op) deck3 3 reversed
        else do
          if (cartaValida (pegaUma pilha) topo) then do
               putStrLn (showCard (pegaUma pilha) ++ "jogada\n")
               if (reversed == True)
                 then do rodarJogo (pegaUma pilha) (tiraUma pilha) deck1 deck2 deck3 1 reversed
               else do rodarJogo (pegaUma pilha) (tiraUma pilha) deck1 deck2 deck3 3 reversed
          else do
            putStrLn ("uma carta foi adicionada na mão de Lula\n")
            if (reversed == True) then do
              rodarJogo topo (tiraUma pilha) deck1 (deck2 ++ [pegaUma pilha]) deck3 1 reversed
            else do rodarJogo topo (tiraUma pilha) deck1 (deck2 ++ [pegaUma pilha]) deck3 3 reversed

gerenciaBot2 :: Carta -> Deck ->  Deck ->  Deck ->  Deck -> Bool -> IO ()
gerenciaBot2 topo pilha deck1 deck2 deck3 reversed = do
  putStrLn ("     Dilmãe está jogando...\n")
  threadDelay 1500000
  if (podeJogar deck3 topo) then do
    if (reversed == True) then do
      let op = escolheJogada deck3 deck2 topo
      if (getEffect(getCarta deck3 op) == "REVERSE") then do
        rodarJogo (getCarta deck3 op) pilha deck1 deck2 (pickPlay deck3 op) 1 False
      else if (getEffect(getCarta deck3 op) == "BLOCK") then do
        msgBlock 3 reversed
        rodarJogo (getCarta deck3 op) pilha deck1 deck2 (pickPlay deck3 op) 1 reversed
      else if (getEffect(getCarta deck3 op) == "+2") then do
        rodarJogo (getCarta deck3 op) (tiraDuas pilha) deck1 (deck2++(pegaDuas pilha)) (pickPlay deck3 op) 2 reversed
      else if (getEffect(getCarta deck3 op) == "+4") then do -- carta +4
        let newColor = selecionaCor deck3
        rodarJogo (setColor newColor (getCarta deck3 op)) (tiraQuatro pilha) deck1 (deck2++(pegaQuatro pilha)) (pickPlay deck3 op) 2 reversed
      else if (getEffect(getCarta deck3 op) == "newColor") then do -- carta nova Cor
        let newColor = selecionaCor deck3
        rodarJogo (setColor newColor (getCarta deck3 op)) pilha deck1 deck2 (pickPlay deck3 op) 1 reversed
      else do rodarJogo (getCarta deck3 op) pilha deck1 deck2 (pickPlay deck3 op) 2 reversed
    else do
      let op = escolheJogada deck3 deck1 topo
      if (getEffect(getCarta deck3 op) == "REVERSE") then do
        rodarJogo (getCarta deck3 op) pilha deck1 deck2 (pickPlay deck3 op) 2 True
      else if (getEffect(getCarta deck3 op) == "BLOCK") then do
        msgBlock 3 reversed
        rodarJogo (getCarta deck3 op) pilha deck1 deck2 (pickPlay deck3 op) 2 reversed
      else if (getEffect(getCarta deck3 op) == "+2") then do
        rodarJogo (getCarta deck3 op) (tiraDuas pilha) (deck1++(pegaDuas pilha)) deck2 (pickPlay deck3 op) 1 reversed
      else if (getEffect(getCarta deck3 op) == "+4") then do -- carta +4
        let newColor = selecionaCor deck3
        rodarJogo (setColor newColor (getCarta deck3 op)) (tiraQuatro pilha) (deck1++(pegaQuatro pilha)) deck2 (pickPlay deck3 op) 1 reversed
      else if (getEffect(getCarta deck3 op) == "newColor") then do -- carta nova Cor
        let newColor = selecionaCor deck3
        rodarJogo (setColor newColor (getCarta deck3 op)) pilha deck1 deck2 (pickPlay deck3 op) 1 reversed
      else do rodarJogo (getCarta deck3 op) pilha deck1 deck2 (pickPlay deck3 op) 1 reversed
  else do
    if (cartaValida (pegaUma pilha) topo) then do
         putStrLn (showCard (pegaUma pilha) ++ "jogada\n")
         if (reversed == True)
           then do rodarJogo (pegaUma pilha) (tiraUma pilha) deck1 deck2 deck3 2 reversed
         else do rodarJogo (pegaUma pilha) (tiraUma pilha) deck1 deck2 deck3 1 reversed
    else do
      putStrLn ("uma carta foi adicionada na mão de Dilmãe\n")
      if (reversed == True) then do
        rodarJogo topo (tiraUma pilha) deck1 deck2 (deck3 ++ [pegaUma pilha]) 2 reversed
      else do rodarJogo topo (tiraUma pilha) deck1 (deck3 ++ [pegaUma pilha]) deck3 1 reversed
