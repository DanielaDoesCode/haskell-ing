module Main (
    main
) where

import System.Environment
import Data.List
import Labirintos
import Test
import System.Directory ( doesFileExist )
{-
Princípios de Programação 2022/2023
Trabalho 4 - Modelo de submissão

* A vossa submissão deverá ser composta por um único ficheiro zip
t4_XXXXX_YYYYY.zip onde XXXXX, YYYYY são os vossos números de aluno
por ordem crescente.
* O ficheiro zip deverá conter no mínimo um ficheiro com o nome Main.hs
* O vosso código deverá ser compilável com uma instrução do tipo

$ stack ghc Main.hs

A instrução acima produz um executável Main, que deverá ser executável
através de um dos seguintes três tipos de instruções:

$ ./Main [ficheiro] -- carrega um ficheiro para jogar
$ ./Main            -- carrega o ficheiro default.map
$ ./Main -t         -- corre os testes
-}

main :: IO ()
main = do
    args <- getArgs
    if length args > 1 then do
        putStrLn "Utilização:"
        putStrLn "./Main [ficheiro] -- carrega um ficheiro para jogar"
        putStrLn "./Main -- carrega o ficheiro default.map"
        putStrLn "./Main -t -- corre os testes"
    else do
        let fileName = if null args then "default.map" else head args
        if fileName ==  "-t" then runTests--runTest 
        else do
            exists <- doesFileExist fileName
            if exists then do
                gameState <- loadGame fileName
                playGame gameState
            else
                putStrLn "Ficheiro não existe!!!"

playGame :: EstadoJogo -> IO()
playGame gameState = do
    print gameState
    input <- getLine
    let (command:toDo) = words input
    if length toDo > 1 then do
        putStrLn "Número de argumentos inválido"
        playGame gameState
    else
        case command of
            "exit" -> return()
            "move" -> do
                if validMoves (head toDo) then playGame $ move gameState (head toDo) else playGame gameState
            "load" -> do
                exists <- doesFileExist (head toDo)
                if exists then do
                    gameState <- loadGame (head toDo)
                    playGame gameState
                else
                    putStrLn "Ficheiro não existe!!!"
            "save" -> do
                saveGame (head toDo) gameState
                playGame gameState
            _ -> do
                putStrLn "Comandos válidos:"
                putStrLn " > move [percurso] -- move o jogador no percurso"
                putStrLn " > load [ficheiro] -- carrega o jogo guardado no ficheiro"
                putStrLn " > save [ficheiro] -- guarda o jogo no ficheiro"
                putStrLn " >  exit -- sai do jogo"
                playGame gameState

validMoves :: String -> Bool
validMoves = foldr ((&&) . validMove) True

validMove :: Char -> Bool
validMove m = m `elem` ['u','d','r','l']

loadGame :: String -> IO EstadoJogo
loadGame fileName = do
    contents <- readFile fileName
    return $ loadGameState contents

saveGame :: String -> EstadoJogo -> IO()
saveGame fileName gameState = do
    writeFile fileName (saveGameState gameState)



