module Test (
    --Movimentos(..),
    runTests,
    prop_same_size_maze,
    prop_player_doesnt_leave,
    prop_no_less_keys,
    prop_no_more_doors,
    prop_no_more_keys,
    prop_same_portals,
    prop_only_one_player,
    prop_same_wall_amount,
) where

import Control.Monad
import Test.QuickCheck
import Labirintos
import Data.List

newtype Movimentos = Movimentos String

movimento :: Movimentos -> String
movimento (Movimentos move) = move

-- instance Arbitrary Movimentos where
--     arbitrary = elements [Movimentos "u",Movimentos "d",Movimentos "r",Movimentos "l"]

instance Arbitrary Movimentos where
    arbitrary = do
        numberMoves <- choose (1,15) :: Gen Int
        moves <- getMoves numberMoves
        return $ Movimentos moves

instance Show Movimentos where
    show (Movimentos moves) = moves

seeGen :: EstadoJogo -> Bool
seeGen _ = True

getMoves :: Int -> Gen String
getMoves amount = do
    move <- replicateM amount (choose (0, 3) :: Gen Int)
    return $ map (\x -> case x of
                            0 -> 'u'
                            1 -> 'd'
                            2 -> 'r'
                            _ -> 'l') move

instance Arbitrary EstadoJogo where
    arbitrary = do
        numLines <- choose (4, 15) :: Gen Int
        numColumns <- choose (4, 15) :: Gen Int
        maze <- getMaze numLines numColumns
        playerKeys <- randomKeys
        return $ inicializa maze playerKeys

randomKeys :: Gen String
randomKeys = do
    amount <- choose (0,3) :: Gen Int
    keys <- replicateM amount (choose ('a', 'c') :: Gen Char)
    return keys

getMaze :: Int -> Int -> Gen [String]
getMaze numL numC = do
    let edge = replicate numC '*'
    let tiles = [' ', '*', 'A', 'B', 'C', 'a', 'b', 'c']
    let tiles2 = "***  a  b  c  A  B  C"
    nucleo <- getNucleo (numL-2) (numC-2) [] tiles2
    nucleo2 <- chooseStartAndFinal (unlines nucleo)
    finalNucleo <- putPortals nucleo2
    let lab = [edge] ++ putBorders finalNucleo ++ [edge]
    return lab

putBorders :: [String] -> [String]
putBorders = map (\x -> "*" ++ x ++ "*")

getNucleo :: Int -> Int  -> [String] -> [Char] -> Gen [String]
getNucleo numL numC lab tiles = do
    if length lab == numL then return lab
    else do
        newLine <- createLine numC tiles
        -- let newTiles = updateTiles tiles newLine 0
        let newLab = lab ++ [newLine]
        getNucleo numL numC newLab tiles


createLine :: Int -> [Char] -> Gen String
createLine numC tiles = do
    replicateM numC (elements tiles)

-- updateTiles :: String -> String -> Int -> String
-- updateTiles tiles line contador
--     | contador == length line = tiles
--     | elem tile line = updateTiles (tiles \\ [tile]) line (contador+1)
--     | otherwise = updateTiles tiles line (contador+1)
--     where tile = tiles!!contador

putPortals :: String -> Gen [String]
putPortals nucleo = do
    putPortal <- arbitrary :: Gen Bool
    if putPortal then do
        choosePortal nucleo
    else
        return $ lines nucleo

chooseStartAndFinal :: String -> Gen String
chooseStartAndFinal nucleo = do
    s <- choose (0,length nucleo-1) :: Gen Int
    f <- choose (0,length nucleo-1) :: Gen Int
    if s/=f && (notElem (nucleo!!s) "\n" && notElem (nucleo!!f) "\n") then do
        let before1 = take s nucleo
        let after1 = drop (s+1) nucleo
        let newNucleo = before1++"S"++after1
        let before2 = take f newNucleo
        let after2 = drop (f+1) newNucleo
        let finalNucleo = before2++"F"++after2
        return finalNucleo
    else do
        chooseStartAndFinal nucleo

choosePortal :: String -> Gen [String]
choosePortal nucleo = do
    p1 <- choose (0,length nucleo-1) :: Gen Int
    p2 <- choose (0,length nucleo-1) :: Gen Int
    if p1/=p2 && (notElem (nucleo!!p1) "SF\n" && notElem (nucleo!!p2) "SF\n") then do
        let before1 = take p1 nucleo
        let after1 = drop (p1+1) nucleo
        let newNucleo = before1++"@"++after1
        let before2 = take p2 newNucleo
        let after2 = drop (p2+1) newNucleo
        let finalNucleo = before2++"@"++after2
        return $ lines finalNucleo
    else do
        choosePortal nucleo

----------------------- // Testes // -----------------------

runTests :: IO()
runTests = do
    quickCheck prop_same_size_maze
    quickCheck prop_player_doesnt_leave
    quickCheck prop_no_less_keys
    quickCheck prop_no_more_doors
    quickCheck prop_no_more_keys
    quickCheck prop_same_portals
    quickCheck prop_only_one_player
    quickCheck prop_same_wall_amount


-- Um teste que verifique a propriedade:
-- as dimensões de um labirinto não mudam após efetuar uma sequência de movimentos.
prop_same_size_maze :: EstadoJogo -> Movimentos -> Bool
prop_same_size_maze gameState (Movimentos moves) = sameLines && sameColumns
    where newGameState = move gameState moves
          newLab = labirinto newGameState
          lab = labirinto gameState
          sameLines = length lab == length newLab
          sameColumns = length (head lab) == length (head newLab)

-- Um teste que verifique a propriedade:
-- o jogador não sai dos limites do mapa após efetuar uma sequência de movimentos.
prop_player_doesnt_leave :: EstadoJogo -> Movimentos -> Bool
prop_player_doesnt_leave gameState (Movimentos moves) = validPlayerPos playerPos dimensions
    where newGameState = move gameState moves
          playerPos = jogador newGameState
          dimensions = labDimensions newGameState

validPlayerPos :: (Int, Int) -> (Int, Int) -> Bool
validPlayerPos (x,y) (lines, columns) = validLine && validColumn
    where validLine = x >= 0 && x < lines
          validColumn = y >= 0 && y < columns          

-- Um teste que verifique a propriedade:
-- o número de chaves na posse do jogador não diminui após efetuar uma sequência de movimentos.
prop_no_less_keys :: EstadoJogo -> Movimentos -> Bool
prop_no_less_keys gameState (Movimentos moves) = length originalKeys <= length newKeys
    where newGameState = move gameState moves
          originalKeys = chaves gameState
          newKeys = chaves newGameState

-- Um teste que verifique a propriedade:
-- o número de portas presentes num labirinto não aumenta após efetuar uma sequência de movimentos. 
prop_no_more_doors :: EstadoJogo -> Movimentos -> Bool
prop_no_more_doors gameState (Movimentos moves) = originalDoors >= newDoors
    where newGameState = move gameState moves
          originalDoors = countTiles gameState ['A'..'C']
          newDoors = countTiles gameState ['A'..'C']

-- Um teste que verifique a propriedade:
-- o número de chaves presentes num labirinto não aumenta após efetuar uma sequência de movimentos.
prop_no_more_keys :: EstadoJogo -> Movimentos -> Bool
prop_no_more_keys gameState (Movimentos moves) = originalKeys >= newKeys
    where newGameState = move gameState moves
          originalKeys = countTiles gameState ['a'..'c']
          newKeys = countTiles gameState ['a'..'c']

-- Um teste que verifique a propriedade:
-- o número de portais presentes num labirinto é o mesmo após efetuar uma sequência de movimentos. 
prop_same_portals :: EstadoJogo -> Movimentos -> Bool
prop_same_portals gameState (Movimentos moves) = originalPortals == newPortals
    where newGameState = move gameState moves
          originalPortals = countTiles gameState ['@']
          newPortals = countTiles gameState ['@']

-- Um teste que verifique a propriedade:
-- o número de jogadores presentes num labirinto é sempre um após uma sequÊncia de movimentos. 
prop_only_one_player :: EstadoJogo -> Movimentos -> Bool
prop_only_one_player gameState (Movimentos moves) = amountPlayers == 1
    where newGameState = move gameState moves
          amountPlayers = countPlayers gameState ['P']

-- Um teste que verifique a propriedade:
-- o número de paredes presentes num labirinto é sempre o mesmo após uma sequência de movimentos. 
prop_same_wall_amount :: EstadoJogo -> Movimentos -> Bool
prop_same_wall_amount gameState (Movimentos moves) = originalWalls == newWalls
    where newGameState = move gameState moves
          originalWalls = countPlayers gameState ['*']
          newWalls = countTiles newGameState ['*']



