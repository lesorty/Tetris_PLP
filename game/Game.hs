-- let grid = variavel
-- int 1 = moveLeft. int 2 = moveRight. int 3 = desce. int 4 = rotate. int 5 = fullFall.
actionLoop :: [[Int]] -> Int -> [[Int]]
-- actionloop grid input =
--  showGrid grid
--  if isGameOver
--     showGameOver
--  if input == 1:
--      if canMove 1:
--          return moveActiveBlocks 1
--     else
--            return grid
--  if input == 2:
--      if canMove 2:
--          return moveActiveBlocks 2
--     else
--            return grid
--  if input == 3:
--      if canFall:
--          moveActiveBlocks 3
--      else        
--          goToNextCycle
--  if input == 4
--      rotate
--  if input == 5
--      fullFall
--      

-- a descida automatica fica aqui tbm


-- quando a peça cai no chão, chama tudo relevante
goToNextCycle :: [[Int]] -> [[Int]]
-- groundBlocks
-- clearMatrix
-- putRandomTetromino