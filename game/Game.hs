import Screen
import MatrixController

actionLoop :: [[Square]] -> Int -> [[Square]]
actionLoop grid input 
    | isGameOver grid = showGameOver
    | input == 1 = if canMove grid 1 then moveActiveBlocks grid 1 else grid
    | input == 2 = if canMove grid 2 then moveActiveBlocks grid 2 else grid
    | input == 3 = if canMove grid 3 then moveActiveBlocks grid 3 else goToNextCycle grid
    | input == 4 = rotate grid
    | input == 5 = fullFall grid

goToNextCycle :: [[Square]] -> [[Square]]
goToNextCycle grid = putRandomTetromino . clearMatrix . groundBlocks grid