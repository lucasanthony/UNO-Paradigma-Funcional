module Bot where

import CartaFunctions
import DeckFunctions

{- Função que recebe o deck do bot e o size do proximo jogador,
verifica se o jogo está perigoso ou não, e então retorna um Int
que representará a posição da carta a ser jogada do seu deck, a
partir daí, a função gerenciaBot assume o comando -}

escolheJogada :: Deck -> Deck -> Carta -> Int
escolheJogada deckBot deckPlayer topo | length deckPlayer <= 2 = lateGame deckBot topo
                                      | length deckPlayer <= 4 = midGame deckBot topo
                                      | otherwise = earlyGame deckBot topo
   

earlyGame :: Deck -> Carta -> Int
earlyGame deckBot  topo = do
    if (firstNormalCard deckBot topo 1 <= length deckBot) then do
        (firstNormalCard deckBot topo 1) - 1
    else do
        firstValidCard deckBot topo 0

midGame :: Deck -> Carta -> Int
midGame deckBot  topo = do
    if (firstMidCard deckBot topo 1 <= length deckBot) then do
        (firstMidCard deckBot topo 1) - 1
    else do
        earlyGame deckBot topo

lateGame :: Deck -> Carta -> Int
lateGame deckBot topo = do
    if(firstLateCard deckBot topo 1 <= length deckBot) then do
        (firstLateCard deckBot topo 1) - 1
    else do
        midGame deckBot topo

selecionaCor :: Deck -> String
selecionaCor deck
    | blueCount deck >= maximum[redCount deck, yellowCount deck, greenCount deck] = "AZUL"
    | redCount deck >= maximum[blueCount deck, yellowCount deck, greenCount deck] = "VERMELHA"
    | yellowCount deck >= maximum[redCount deck, blueCount deck, greenCount deck] = "AMARELA"
    | greenCount deck >= maximum[redCount deck, blueCount deck, yellowCount deck] = "VERDE"
    | otherwise = "PRETA" 

blueCount :: Deck -> Int
blueCount [] = 0
blueCount (x:xs) = if getColor x == "AZUL" then 1+blueCount xs else blueCount xs

redCount :: Deck -> Int
redCount [] = 0
redCount (x:xs) = if getColor x == "VERMELHA" then 1+redCount xs else redCount xs

yellowCount :: Deck -> Int
yellowCount [] = 0
yellowCount (x:xs) = if getColor x == "AMARELA" then 1+yellowCount xs else yellowCount xs

greenCount :: Deck -> Int
greenCount [] = 0
greenCount (x:xs) = if getColor x == "Verde" then 1+greenCount xs else greenCount xs



                                                           
    
                                                   

