module Labirintos (
    EstadoJogo(..),
    inicializa,
    loadGameState,
    saveGameState,
    jogador,
    chaves,
    terminado,
    labirinto,
    labDimensions,
    countTiles,
    countPlayers,
    move,
) where
import Data.Char
import Data.List

data EstadoJogo = EstadoJogo [String] String (Int,Int) [(Int,Int)] Bool

-- instance Show EstadoJogo where
--     show (EstadoJogo lab chaves player portal terminado) = labP ++ "chaves: " ++ chaves
--         where lab1 = unlines lab
--               before = take (length lab*x + y) lab1
--               after = drop (length lab*x + y + 1) lab1
--               labP = before++"P"++after
--               (x,y) = player

instance Show EstadoJogo where
     show (EstadoJogo lab chaves player portal terminado) = labP ++ "chaves: " ++ chaves
        where labP = unlines $ getLabWithPlayer player lab

getLabWithPlayer :: (Int,Int) -> [String] -> [String]
getLabWithPlayer (x,y) lab = before ++ [replaceWithPlayer line y] ++ after
    where line = lab!!x
          before = take x lab
          after = drop (x+1) lab

replaceWithPlayer :: String -> Int -> String
replaceWithPlayer line index = before ++ "P" ++ after
    where before = take index line
          after = drop (index+1) line


inicializa :: [String] -> String -> EstadoJogo
inicializa lab chaves = EstadoJogo lab chaves (head (getPosByChar lab (0,0) [] 'S')) (getPosByChar lab (0,0) [] '@') False

-- inicializa :: [String] -> EstadoJogo
-- inicializa lab = EstadoJogo lab [] (1,1) [] False

startGame :: (Int,Int) -> String -> [String] -> EstadoJogo
startGame player chaves lab = EstadoJogo lab chaves player (getPosByChar lab (0,0) [] '@') False

loadGameState :: String -> EstadoJogo
loadGameState contents = gameState
    where (player:key:lab) = lines contents
          gameState = startGame (read player) key lab

saveGameState :: EstadoJogo -> String
saveGameState (EstadoJogo lab chaves player _ _) = show player++"\n"++chaves++"\n"++unlines lab++"\n"

getPosByChar :: [String] -> (Int,Int) -> [(Int,Int)] -> Char -> [(Int,Int)]
getPosByChar lab (l,c) list key
    | l == length lab = list
    | c == length (lab!!0) = getPosByChar lab (l+1,0) list key
    | (lab!!l)!!c == key = getPosByChar lab (l,c+1) (list++[(l,c)]) key
    | otherwise = getPosByChar lab (l,c+1) list key

jogador:: EstadoJogo -> (Int,Int)
jogador (EstadoJogo _ _ player _ _) = player

chaves :: EstadoJogo -> String
chaves (EstadoJogo _ chaves _ _ _) = chaves

terminado:: EstadoJogo -> Bool
terminado (EstadoJogo _ _ _ _ terminated) = terminated

labirinto :: EstadoJogo -> [String]
labirinto (EstadoJogo lab _ _ _ _) = lab

labDimensions :: EstadoJogo -> (Int,Int)
labDimensions (EstadoJogo lab _ _ _ _) = (lines,columns)
    where lines = length lab
          columns = length (head lab)

countTiles :: EstadoJogo -> [Char] -> Int
countTiles (EstadoJogo lab _ _ _ _) target = foldl (\count xs -> count+countLetters xs target) 0 lab

countPlayers :: EstadoJogo -> [Char] -> Int
countPlayers gameState target = foldl (\count line -> count+countLetters line target) 0 labP
    where labP = init $ lines $ show gameState

countLetters :: String -> [Char] -> Int
countLetters line target = foldl (\count char -> if elem char target then (count + 1) else count) 0 line

move :: EstadoJogo -> String -> EstadoJogo
move estado "" = estado
move estado (x:xs) = move (moveAux estado x) xs

moveAux :: EstadoJogo -> Char -> EstadoJogo
moveAux (EstadoJogo lab chaves player portais terminado) movimento
    | casa == ' ' = EstadoJogo lab chaves newPlayer portais False
    | casa == 'F' = EstadoJogo lab chaves newPlayer portais True
    | casa == 'S' = EstadoJogo lab chaves newPlayer portais False
    | elem casa ['a'..'c'] = EstadoJogo (removeKey lab casa) (addKey casa chaves) newPlayer portais False
    | elem casa ['A'..'C'] && elem (toLower casa) chaves = EstadoJogo (removeKey lab casa) chaves newPlayer portais False
    | casa == '@' = EstadoJogo lab chaves (teleport lab newPlayer portais) portais False
    | otherwise = EstadoJogo lab chaves player portais False
    where vetor = convert movimento
          newPlayer = (fst player + fst vetor, snd player + snd vetor)
          casa = (lab!!fst newPlayer)!!snd newPlayer

addKey:: Char -> String -> String
addKey c chaves
    | elem c chaves = sort (c:chaves)
    | otherwise = chaves


teleport::[String] -> (Int,Int) -> [(Int,Int)] ->(Int,Int)
teleport lab player portais = if head portais == player then last portais else head portais

removeKey :: [String] -> Char -> [String]
removeKey lab key = map (map(\x -> if x == key then ' ' else x )) lab

convert:: Char ->(Int,Int)
convert dir
    |dir == 'u' = (-1,0)
    |dir == 'd' = (1,0)
    |dir == 'l' = (0,-1)
    |otherwise = (0,1)
