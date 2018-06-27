module CartaFunctions where

type Numero = Int
type Cor = String
type Efeito = String
type Carta = (Numero, Cor, Efeito)

-- Retorna a cor da carta
getColor :: Carta -> Cor
getColor (_,cor,_) = cor

-- Retorna o efeito da carta
getEffect :: Carta -> Efeito
getEffect (_,_,efeito) = efeito

-- Retorna o número da carta
getNumber :: Carta -> Numero
getNumber (n,_,_) = n

-- Exibe a carta do topo
showTopo :: Carta -> IO()
showTopo s = do
    putStrLn ("Topo : " ++ "Numero: " ++ show(getNumber s) ++ " Cor: " ++ getColor s ++ " Efeito: " ++ getEffect s ++ "\n")

showCard :: Carta -> String
showCard s = ("A carta : " ++ "Numero: " ++ show(getNumber s) ++ " Cor: " ++ getColor s ++ " Efeito: " ++ getEffect s ++ " foi ")

-- Verifica se a carta escolhida é válida
cartaValida :: Carta -> Carta -> Bool
cartaValida carta topo | getColor topo == getColor carta || getColor topo == "first card" || getNumber topo == getNumber carta || ((getEffect topo == getEffect carta) && (getEffect carta /= " ")) = True
                       | otherwise = False

msgBlock :: Int -> Bool -> IO()
msgBlock vez reversed | (vez == 1 || vez == 3) && reversed == True = putStrLn ("Dilmãe passa a vez!!")
                      | (vez == 1 || vez == 2) && reversed == False = putStrLn ("Lula passa a vez!!")
                      | otherwise = putStrLn ("Você passa a vez!!")
