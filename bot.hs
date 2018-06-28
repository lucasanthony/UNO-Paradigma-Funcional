module Bot where

import CartaFunctions
import DeckFunctions

{- Função que recebe o deck do bot e o size do proximo jogador,
verifica se o jogo está perigoso ou não, e então retorna um Int
que representará a posição da carta a ser jogada do seu deck, a
partir daí, a função gerenciaBot assume o comando -}

escolheJogada :: Deck -> Deck -> Carta -> Int
escolheJogada deckBot deckPlayer topo = do
  if (size deckPlayer <= 3) then do
    if (length (specialCards deckBot [] topo 0) > 0) then do
      head (specialCards deckBot [] topo 0)
    else do
      firstCardValid deckBot topo 0
  else do
   firstCardValid deckBot topo 0
