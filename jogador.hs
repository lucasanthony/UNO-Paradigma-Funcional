module JogadorFunctions where

import CartaFunctions
import DeckFunctions

data Jogador = Jogador Nome Pontuacao Mao
                  deriving (Show, Read)
type Jogadores = [Jogador]
type Nome = String
type Pontuacao = Int
type Mao = [Carta]

-- função que recebe um jogador e retorna o nome
obterNome :: Jogador -> Nome
obterNome (Jogador nome _ _) = nome

-- Mostra ao jogador o próximo bot a jogar
next :: Bool -> String
next reversed | reversed == True = "Próximo a jogar: Dilmãe\n"
                      | otherwise = "Próximo a jogar: Lula\n"

-- Mostra ao jogador quantas cartas cada bot tem na mão
status :: Deck -> Deck -> IO()
status deck2 deck3 = putStrLn ("Status: Lula -> " ++ show (size deck2) ++ " cartas; Dilmãe -> " ++ show (size deck3) ++ " cartas\n")
