data Color = Blue | Cyan | Orange | Yellow | Green | Violet | Red deriving Eq
data Piece = None | LeftL | RightL | Square | Rectangule | LeftS | RigthS | T deriving Eq
data Falling = Enable | Disable deriving Eq


-- pronta
findIndexes :: Int -> [[Int]] -> [(Int, Int)]
findIndexes s matrix = do
-- iterate by the rows and columns to get the i and j of the elements
  (i, row) <- zip [0..] matrix
  (j, elem) <- zip [0..] row
  if elem == s then return (i, j) else []

-- FALTA MODULARIZAR PARA OS LADOS
canMove :: [[Int]] -> Bool
canMove matrix coordinate = if bellow == 2 then False else True
    where bellow = matrix !! ((fst coordinate) + 1) !! (snd coordinate) 

-- PARA CONSULTA
--canFallDownBlock :: [[Int]] -> [(Int,Int)] -> Bool
--canFallDownBlock matrix [x] =  canFallDown matrix (fst x, snd x)
--canFallDownBlock matrix (x:xs) =
--    if canFallDown matrix (fst x, snd x) then  canFallDownBlock matrix xs
--    else False
-- pega a grid, o conjunto de coordenadas ativas e uma direção. retorna se elas podem ir pra uma direçao
canMoveTetromino :: [[Int]] -> [(Int,Int)] -> Int -> Bool


-- pronta
fallTetromino :: [[Int]] -> [(Int,Int)] -> [[Int]]
fallTetromino :: [[Int]] -> [(Int,Int)] -> [(Int,Int)]
fallTetromino matrix [x] = [((fst x + 1), snd x)]
fallTetromino matrix (x:xs) = ((fst x + 1), snd x) : fallTetromino matrix xs

-- S
-- Pega uma matriz e retorna a lista de linhas que podem ser limpas
clearableLines :: [[Int]] -> [Int]
-- usa canClearLine

--S
-- pega uma matriz e um índice. retorna se essa linha é clearável ou não
canClearLine :: [[Int]] -> Int -> Bool

--S
-- pega uma matriz e uma lista de índices. retorna uma matriz com todos esses índices clearados
clearTheseLines :: [[Int]] -> [Int] -> [[Int]]

-- S
-- pega uma matriz e retorna ela com todas as linhas clearáveis clearadas
clearMatrix :: [[Int]] -> ([[Int]], Int)
-- clearMatrix grid = 
    -- x = clearableLines grid
    -- clearedMatrix = clearTheseLines grid x
    -- return clearedMatrix 

-- L
-- pega uma matriz e um sentido. retorna essa matriz com os blocos ativos rotacionados pra direita
rotate :: [[Int]] -> [[Int]]

-- L
--pega as coordenadas de cada bloco de um tetromino. retorna as coordenadas finais que cada bloco deve ter.
getRotationEndPositions :: [(Int,Int)] -> [(Int,Int)]

-- E
--retorna se um conjunto de blocos pode ser colocado na grid.
canBePut :: [[Int]] -> [(Int, Int)] -> Bool

-- 
--retorna 1 se pode ser posto com um movimento pra esquerda. 2 com um pra direita. 0 se não pode
canBePutWithSideMove :: [[Int]] -> [(Int, Int)] -> Int

--pega um conjunto de coordenadas. sobe elas até que possa botar elas na matriz, então bota elas na matriz
raiseUntilAllowed :: [[Int]] -> [(Int, Int)] -> [[Int]]

--retorna true se tem algum bloco acima do limite da grid, false caso contrário
isGameOver :: [[Int]] -> Bool

--usa magia de Gloss para exibir a grid na tela
showGrid :: [[Int]] -> ????

[0,0,1],[0,2,0]
001
020

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