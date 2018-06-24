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
