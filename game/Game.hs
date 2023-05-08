import Screen
import MatrixController

-- TO TEST
actionLoop :: [[Square]] -> Move -> [[Square]]
actionLoop matrix input 
    | isGameOver matrix = showGameOver
    | input == (Move Left) = if canMoveTetromino matrix (Move Left) then moveTetromino matrix (Move Left) else matrix
    | input == (Move Right) = if canMoveTetromino matrix (Move Right) then moveTetromino matrix (Move Right) else matrix
    | input == (Move Down) = if canMoveTetromino matrix (Move Down) then moveTetromino matrix (Move Down) else goToNextCycle matrix
    | input == (Move Rotate) = rotate matrix
    | input == (Move SuperDown) = fullFall matrix


-- TO TEST
goToNextCycle :: [[Square]] -> [[Square]]
goToNextCycle matrix = putRandomTetromino . clearMatrix . groundBlocks matrix

inputToMove :: Event -> Move
inputToMove (EventKey (Char 'a') Down _ _) = MoveLeft
inputToMove (EventKey (Char 'd') Down _ _) = MoveRight
inputToMove (EventKey (Char 'w') Down _ _) = MoveRotate
inputToMove (EventKey (Char 's') Down _ _) = MoveDown
inputToMove (EventKey (SpecialKey KeySpace) Down _ _) = SuperBaixo
inputToMove _ = MoveNone