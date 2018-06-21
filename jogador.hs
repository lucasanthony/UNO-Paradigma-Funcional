module JogadorFunctions where

import CartaFunctions
import Data.List
import Data.Function

data Jogador = Jogador Nome Pontuacao Mao
					deriving (Show, Read)
type Jogadores = [Jogador]
type Nome = String
type Pontuacao = Int
type Mao = [Carta]

atualizaPontuacao :: Jogadores -> String -> Jogadores
atualizaPontuacao ((Jogador nome pontuacao mao):xs) vencedor
         | (nome == vencedor) = [(Jogador nome (pontuacao + 1) mao)] ++ xs
         | otherwise = (Jogador nome pontuacao mao):(atualizaPontuacao xs vencedor)

-- exibir ranking dos jogadores
-- critério: da maior para a menor pontuação
exibirRanking :: Jogadores -> IO ()
exibirRanking [] = return ()
exibirRanking (x:xs) = do
      putStrLn ((obterNome x) ++ " possui " ++ (show (obterPontuacao x)) ++ " vitorias")
      exibirRanking xs

-- função que recebe um jogador e retorna o nome
obterNome :: Jogador -> Nome
obterNome (Jogador nome _ _) = nome

-- função que recebe um jogador e retorna a pontuação
obterPontuacao :: Jogador -> Pontuacao
obterPontuacao (Jogador _ pontuacao _) = pontuacao

-- função que define o critério de ordenação
ordenar :: Jogadores -> Jogadores
ordenar dados = sortBy (compare `on` obterPontuacao) dados

-- Verifica se o nome do jogador já está cadastrado
existeJogador :: Jogadores -> Nome -> Bool
existeJogador [] _ = False
existeJogador ((Jogador n p m):xs) nome
      | (n == nome) = True
      | otherwise = existeJogador xs nome
