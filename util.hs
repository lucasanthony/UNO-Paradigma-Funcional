module Util where

showLines :: [String] -> IO()
showLines [] = return()
showLines s = do
    putStrLn (head s)
    showLines (tail s)

-- Exibe a tela principal do game
tela_principal :: IO()
tela_principal = do
    cont <- readFile ".msg"
    showLines(take 37 (lines cont))

showRules :: IO()
showRules = do
    cont <- readFile "regras.msg"
    showLines(take 18 (lines cont))

main :: IO()
main = do
    tela_principal

-- Limpa a tela
cleanScreen :: IO()
cleanScreen = putStr "\ESC[1J"

checkInput :: String -> Bool
checkInput input | input == " " || input == "\n" = False
                 | input < "ESC" = True
                 | otherwise = True
