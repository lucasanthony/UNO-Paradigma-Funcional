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

-- Limpa a tela
cleanScreen :: IO()
cleanScreen = putStr "\ESC[1J"

checkInput :: String -> Bool
checkInput input | input == "0" || input == "1" || input == "2" || input == "3" || input == "4" || input == "5" || input == "6" || input == "7" || input == "8" || input == "9" || input == "10" || input == "11" || input == "12" || input == "13" || input == "14" || input == "15" || input == "16" = True       
                 | otherwise = False
