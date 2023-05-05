
findIndices :: Int -> [[Int]] -> [(Int, Int)]

canFall :: [[Int]] -> Bool

canFallTetromino :: [[Int]] -> [(Int,Int)] -> Bool

fallTetromino :: [[Int]] -> [(Int,Int)] -> [[Int]]

-- Pega uma matriz e retorna a lista de linhas que podem ser limpas
clearableLines :: [[Int]] -> [Int]
-- usa canClearLine

-- pega uma matriz e um índice. retorna se essa linha é clearável ou não
canClearLine :: [[Int]] -> Int -> Bool

-- pega uma matriz e uma lista de índices. retorna uma matriz com todos esses índices clearados
clearTheseLines :: [[Int]] -> [Int] -> [[Int]]

-- pega uma matriz e retorna ela com todas as linhas clearáveis clearadas
clearMatrix :: [[Int]] -> [[Int]]
-- clearMatrix grid = 
    -- x = clearableLines grid
    -- clearedMatrix = clearTheseLines grid x
    -- return clearedMatrix 

-- pega uma matriz e um sentido. retorna essa matriz com os blocos ativos rotacionados pra direita
rotate :: [[Int]] -> [[Int]]

--pega as coordenadas de cada bloco de um tetromino. retorna as coordenadas finais que cada bloco deve ter.
getRotationEndPositions :: [(Int,Int)] -> [(Int,Int)]

--retorna se um conjunto de blocos pode ser colocado na grid.
canBePut :: [[Int]] -> [(Int, Int)] -> Bool

--retorna 1 se pode ser posto com um movimento pra esquerda. 2 com um pra direita. 0 se não pode
canBePutWithSideMove :: [[Int]] -> [(Int, Int)] -> Int

--pega um conjunto de coordenadas. sobe elas até que possa botar elas na matriz, então bota elas na matriz
raiseUntilAllowed :: [[Int]] -> [(Int, Int)] -> [[Int]]

--retorna true se tem algum bloco acima do limite da grid, false caso contrário
isGameOver :: [[Int]] -> Bool

--usa magia de Gloss para exibir a grid na tela
showGrid :: [[Int]] -> ????

showGameOver :: ????

--recebe uma grid e uma direção (esq, baixo, dir). retorna se os blocos ativos podem se mover nessa direçao
canMove :: [[Int]] -> Int -> Bool

--move todos os blocos ativos numa direção. presume que eles podem ser movidos
moveActiveBlocks :: [[Int]] -> Int -> [[Int]]

fullFall :: [[Int]] -> [[Int]]

-- retorna a nova grid, com os blocos ativos derrubados pra baixo.
forceFall :: [[Int]]
--  showGrid grid
--  if isGameOver
--     showGameOver
--  if !canFall
--      goToNextCycle

-- desativa todos os blocos
groundBlocks :: [[Int]] -> [[Int]]

getRandomTetromino :: ([(Int, Int)], Int)

-- bota um tetromino aleatório na grid.
putRandomTetromino :: [[Int]] -> [[Int]]

-- quando a peça cai no chão, chama tudo relevante
goToNextCycle :: [[Int]] -> [[Int]]
-- groundBlocks
-- clearMatrix
-- putRandomTetromino


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



