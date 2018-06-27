module DeckFunctions where

import CartaFunctions

type Deck = [Carta]

-- Retorna o tamanho do deck
size :: Deck -> Int
size [] = 0
size (x:xs) = 1 + size xs

-- VERIFICA SE O JOGADOR TEM ALGUMA CARTA Q DA MATCH COM A DO TOPO
podeJogar :: Deck -> Carta -> Bool
podeJogar [] _ = False
podeJogar (x:xs) (n,c,e) | getColor x == c || getNumber x == n || (getEffect x == e && e /= "none") || c == "first card" = True
                         | otherwise = podeJogar xs (n,c,e)

pegaUma :: Deck -> Carta
pegaUma [x] = x
pegaUma (x:xs) = pegaUma xs

tiraUma :: Deck -> Deck
tiraUma [x] = []
tiraUma (x:xs) = [x] ++ tiraUma xs

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
pickPlay :: Deck -> Int -> Deck
pickPlay [] _ = []
pickPlay (_:xs) 0 = xs
pickPlay (x:xs) n | n == 0 = pickPlay xs (n+1)
                 | otherwise = [x] ++ pickPlay xs (n-1)

-- Retorna uma carta específica do deck passado como parâmetro
getCarta :: Deck -> Int -> Carta
getCarta ((n,cor,efeito):xs) x | x == 0 = (n,cor,efeito)
                               | otherwise = getCarta xs (x-1)

-- Mostra as cartas do jogador
showCards :: Deck -> Carta -> Int -> IO()
showCards [] _ _ = return()
showCards s topo n = do
  if (cartaValida (head s) topo) then do
    putStrLn ("> " ++ (show n) ++ " - " ++ "Numero: " ++ show(getNumber(head s)) ++ " Cor: " ++ getColor(head s) ++ " Efeito: " ++ getEffect(head s))
  else do putStrLn ("  " ++ (show n) ++ " - " ++ "Numero: " ++ show(getNumber(head s)) ++ " Cor: " ++ getColor(head s) ++ " Efeito: " ++ getEffect(head s))
  showCards (tail s) topo (n+1)

-- Função que verifica se o player venceu a partida,
-- verificação feita pelo deck do mesmo
venceu :: Deck -> Bool
venceu d | size d == 0 = True
         | otherwise = False

-- Funcão para mostrar o deck
showDeck :: Deck -> Deck
showDeck deck = deck
