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
                                                           
    
                                                   

