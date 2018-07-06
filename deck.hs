module DeckFunctions where

import CartaFunctions

type Deck = [Carta]

-- Retorna o tamanho do deck
size ::  Deck -> Int
size [] = 0
size (x:xs) = 1 + size xs

-- VERIFICA SE O JOGADOR TEM ALGUMA CARTA Q DA MATCH COM A DO TOPO
podeJogar ::  Deck -> Carta -> Bool
podeJogar [] _ = False
podeJogar (x:xs) (n,c,e) | getColor x == c || getNumber x == n || getEffect x == "+4" || getEffect x == "newColor" || (getEffect x == e && e /= " ") || c == "first card" = True
                         | otherwise = podeJogar xs (n,c,e)

--Funcao que retorna 1 carta do deck principal que irao pra mao do player
pegaUma ::  Deck -> Carta
pegaUma [x] = x
pegaUma (x:xs) = pegaUma xs

-- Funcao que retira 1 carta do deck principal para colocar na mao de algum player
-- quando o mesmo nn possuir carta valida
tiraUma ::  Deck ->  Deck
tiraUma [x] = []
tiraUma (x:xs) = [x] ++ tiraUma xs

-- Funcao que retira 2 cartas do deck principal para colocar na mao de algum player
-- quando uma carta +2 for usada
tiraDuas ::  Deck ->  Deck
tiraDuas [x,y] = []
tiraDuas [x] = []
tiraDuas (x:xs) = [x] ++ tiraDuas xs

-- Funcao que retira 4 cartas do deck principal para colocar na mao de algum player
-- quando uma carta +4 for usada
tiraQuatro ::  Deck ->  Deck
tiraQuatro [a,b,c,d] = []
tiraQuatro [a,b,c] = []
tiraQuatro [a,b] = []
tiraQuatro [a] = []
tiraQuatro [] = []
tiraQuatro (x:xs) = [x] ++ tiraQuatro xs

--Funcao que retorna 2 cartas do deck principal que irao pra mao do player
pegaDuas ::  Deck ->  Deck
pegaDuas [x,y] = [x,y]
pegaDuas (x:xs) = pegaDuas xs

--Funcao que retorna 4 cartas do deck principal que irao pra mao do player
pegaQuatro ::  Deck ->  Deck
pegaQuatro [a,b,c,d] = [a,b,c,d]
pegaQuatro (x:xs) = pegaQuatro xs

-- Funcao para retirar uma carta especifica da mao do player
pickPlay ::  Deck -> Int ->  Deck
pickPlay [] _ = []
pickPlay (_:xs) 0 = xs
pickPlay (x:xs) n | n == 0 = pickPlay xs (n+1)
                 | otherwise = [x] ++ pickPlay xs (n-1)

-- Retorna uma carta específica do deck passado como parâmetro
getCarta ::  Deck -> Int -> Carta
getCarta [] x = (0,"  ","  ")
getCarta ((n,cor,efeito):xs) x | x == 0 = (n,cor,efeito)
                               | otherwise = getCarta xs (x-1)

-- Mostra as cartas do jogador, as cartas que podem ser jogadas
-- são indicadas com " >"
showCards ::  Deck -> Carta -> Int -> IO()
showCards [] _ _ = return()
showCards s topo n = do
  if (cartaValida (head s) topo) then do
    if (getColor (head s) == "VERDE") then do
      if(getEffect(head s) == " ") then do
        putStrLn ("> " ++ (show n) ++ " - " ++ "Numero: " ++ show(getNumber(head s)) ++ " Cor: " ++ getColor(head s) ++ "    Efeito: " ++ getEffect(head s))
      else do
        putStrLn ("> " ++ (show n) ++ " - " ++ "          Cor: " ++ getColor(head s) ++ "    Efeito: " ++ getEffect(head s))
    else if (getColor (head s) == "AZUL") then do
      if(getEffect(head s) == " ") then do
        putStrLn ("> " ++ (show n) ++ " - " ++ "Numero: " ++ show(getNumber(head s)) ++ " Cor: " ++ getColor(head s) ++ "    Efeito: " ++ getEffect(head s))
      else do
        putStrLn ("> " ++ (show n) ++ " - " ++ "          Cor: " ++ getColor(head s) ++ "    Efeito: " ++ getEffect(head s))
    else if (getColor (head s) == "AMARELA") then do
      if(getEffect(head s) == " ") then do
        putStrLn ("> " ++ (show n) ++ " - " ++ "Numero: " ++ show(getNumber(head s)) ++ " Cor: " ++ getColor(head s) ++ "    Efeito: " ++ getEffect(head s))
      else do
        putStrLn ("> " ++ (show n) ++ " - " ++ "          Cor: " ++ getColor(head s) ++ "    Efeito: " ++ getEffect(head s))
    else do 
      if(getEffect(head s) == " ") then do
        putStrLn ("> " ++ (show n) ++ " - " ++ "Numero: " ++ show(getNumber(head s)) ++ " Cor: " ++ getColor(head s) ++ "    Efeito: " ++ getEffect(head s))
      else do
        putStrLn ("> " ++ (show n) ++ " - " ++ "          Cor: " ++ getColor(head s) ++ "    Efeito: " ++ getEffect(head s))
  else do
    if (getColor (head s) == "VERDE") then do
      if(getEffect(head s) == " ") then do
        putStrLn ("  " ++ (show n) ++ " - " ++ "Numero: " ++ show(getNumber(head s)) ++ " Cor: " ++ getColor(head s) ++ "       Efeito: " ++ getEffect(head s))
      else do
        putStrLn ("  " ++ (show n) ++ " - " ++ "          Cor: " ++ getColor(head s) ++ "    Efeito: " ++ getEffect(head s))
    else if (getColor (head s) == "AZUL") then do
      if(getEffect(head s) == " ") then do
        putStrLn ("  " ++ (show n) ++ " - " ++ "Numero: " ++ show(getNumber(head s)) ++ " Cor: " ++ getColor(head s) ++ "        Efeito: " ++ getEffect(head s))
      else do
        putStrLn ("  " ++ (show n) ++ " - " ++ "          Cor: " ++ getColor(head s) ++ "    Efeito: " ++ getEffect(head s))
    else if (getColor (head s) == "AMARELA") then do
      if(getEffect(head s) == " ") then do
        putStrLn ("  " ++ (show n) ++ " - " ++ "Numero: " ++ show(getNumber(head s)) ++ " Cor: " ++ getColor(head s) ++ "   Efeito: " ++ getEffect(head s))
      else do
        putStrLn ("  " ++ (show n) ++ " - " ++ "          Cor: " ++ getColor(head s) ++ "    Efeito: " ++ getEffect(head s))
    else do 
      if(getEffect(head s) == " ") then do
        putStrLn ("  " ++ (show n) ++ " - " ++ "Numero: " ++ show(getNumber(head s)) ++ " Cor: " ++ getColor(head s) ++ "      Efeito: " ++ getEffect(head s))
      else do
        putStrLn ("  " ++ (show n) ++ " - " ++ "          Cor: " ++ getColor(head s) ++ "    Efeito: " ++ getEffect(head s))
  showCards (tail s) topo (n+1)

-- Função que verifica se o player venceu a partida,
-- verificação feita pelo deck do mesmo
venceu ::  Deck -> Bool
venceu d | size d == 0 = True
         | otherwise = False

-- Funcão para mostrar o deck
showDeck ::  Deck -> Deck
showDeck deck = deck

-- vefifica se um deck tem carta especial
temSpecialCard ::  Deck -> Bool
temSpecialCard [] = False
temSpecialCard (x:xs) | isSpecialCard x = True
                      | otherwise = temSpecialCard xs

-- retorna as posições das cartas especiais de um deck, se tiver
specialCards ::  Deck -> [Int] -> Carta -> Int -> [Int]
specialCards deck retorno topo pos = do
  if (size deck == 0) then do
    retorno
  else if (cartaValida (head deck) topo && isSpecialCard (head deck)) then do
    let card = head deck
    specialCards (tail deck) (retorno++[pos]) topo (pos+1)
  else do
    specialCards (tail deck) retorno topo (pos+1)

-- primeira carta do deck que pode ser jogada
firstValidCard :: Deck -> Carta -> Int -> Int
firstValidCard [] _ pos = pos
firstValidCard (x:xs) topo pos | cartaValida x topo == True = pos
                               | otherwise = firstValidCard xs topo (pos+1)

-- primeira carta do deck que nao possui efeito
firstNormalCard ::  Deck -> Carta -> Int -> Int
firstNormalCard [] _ pos = pos
firstNormalCard (x:xs) topo pos | cartaValida x topo == True && isSpecialCard x == False = pos
                                | otherwise = firstNormalCard xs topo (pos + 1)
  
-- primeira carta com efeito de Block ou Reverse
firstMidCard ::  Deck -> Carta -> Int -> Int
firstMidCard [] _ pos = pos
firstMidCard (x:xs) topo pos | cartaValida x topo == True && isMidCard x == True = pos
                             | otherwise = firstMidCard xs topo (pos + 1)
                             
-- primeira carta com efeito de +2 ou +4
firstLateCard ::  Deck -> Carta -> Int -> Int
firstLateCard [] _ pos = pos
firstLateCard (x:xs) topo pos | cartaValida x topo == True && isLateCard x == True = pos
                             | otherwise = firstLateCard xs topo (pos + 1)

-- posição de determinada carta no deck
cardPosition ::  Deck -> Int -> Int
cardPosition (x:xs) pos | isSpecialCard x = pos
                        | otherwise = cardPosition xs (pos+1)

insertByIndex :: [a] -> a -> Int -> [a]
insertByIndex [] _ _ = []
insertByIndex (x:xs) element i | i == 0 = [element] ++ insertByIndex xs element (i-1)
                               | otherwise = [x] ++ insertByIndex xs element (i-1)

myShuffle :: [Int] -> [a] -> [a] -> [a]
myShuffle indices deck retorno = do
if (length indices == 0) then do
  retorno
  else do
    let newRetorno = insertByIndex retorno (head deck) (head indices)
    myShuffle (tail indices) (tail deck) newRetorno


{- vermelhas = [(0,"VERMELHA"," "),(1,"VERMELHA"," "),(2,"VERMELHA"," "),(3,"VERMELHA"," "),(4,"VERMELHA"," "),
(5,"VERMELHA"," "),(6,"VERMELHA"," "),(7,"VERMELHA"," "),(8,"VERMELHA"," "),(9,"VERMELHA"," "),
(00,"VERMELHA","BLOCK"),(01,"VERMELHA","REVERSE"),(02,"VERMELHA","+2")]

azuis = [(0,"AZUL"," "),(1,"AZUL"," "),(2,"AZUL"," "),(3,"AZUL"," "),(4,"AZUL"," "),(5,"AZUL"," "),
(6,"AZUL"," "),(7,"AZUL"," "),(8,"AZUL"," "),(9,"AZUL"," "),(00,"AZUL","BLOCK"),(01,"AZUL","REVERSE"),
(02,"AZUL","+2")]

verdes = [(0,"VERDE"," "),(1,"VERDE"," "),(2,"VERDE"," "),(3,"VERDE"," "),(4,"VERDE"," "),(5,"VERDE"," "),
(6,"VERDE"," "),(7,"VERDE"," "),(8,"VERDE"," "),(9,"VERDE"," "),(00,"VERDE","BLOCK"),(01,"VERDE","REVERSE"),
(02,"VERDE","+2")]

amarelas = [(0,"AMARELA"," "),(1,"AMARELA"," "),(2,"AMARELA"," "),(3,"AMARELA"," "),(4,"AMARELA"," "),
(5,"AMARELA"," "),(6,"AMARELA"," "),(7,"AMARELA"," "),(8,"AMARELA"," "),(9,"AMARELA"," "),(00,"AMARELA","BLOCK"),
(01,"AMARELA","REVERSE"),(02,"AMARELA","+2")]

especiais = [(03,"PRETA","+4"),(03,"PRETA","+4"),(03,"PRETA","+4"),(03,"PRETA","+4"),
(04,"PRETA","CORINGA"),(04,"PRETA","CORINGA"),(04,"PRETA","CORINGA"),(04,"PRETA","CORINGA"),]
-}
