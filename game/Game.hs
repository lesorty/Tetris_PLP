import System.IO
import Control.Concurrent
import Screen
import MatrixController

data GameState = GameState {
    matrix :: [[Square]]
    ticksSinceLastDrop :: Int
    score :: Int
}

-- TO TEST
nextBoardState :: [[Square]] -> Move -> [[Square]]
nextBoardState matrix input 
    | isGameOver matrix = showGameOver
    | input == (Move Left) = if canMoveTetromino matrix (Move Left) then moveTetromino matrix (Move Left) else matrix
    | input == (Move Right) = if canMoveTetromino matrix (Move Right) then moveTetromino matrix (Move Right) else matrix
    | input == (Move Down) = if canMoveTetromino matrix (Move Down) then moveTetromino matrix (Move Down) else goToNextCycle matrix
    | input == (Move Rotate) = rotate matrix
    | input == (Move SuperDown) = fullFall matrix


-- TO TEST
goToNextCycle :: [[Square]] -> [[Square]]
goToNextCycle matrix = putRandomTetromino . clearMatrix . groundBlocks matrix

main :: IO ()
main = play window black 30 emptyMatrix showGrid (\event matrix -> nextBoardState matrix (inputToMove event)) (\_ state -> state)
    
  
    




