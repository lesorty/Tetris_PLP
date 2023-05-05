data Move = Left | Right | Rotate | Down | SuperDown
data Color = Blue | Cyan | Orange | Yellow | Green | Violet | Red deriving Eq
data Piece = None | LeftL | RightL | Square | Rectangule | LeftS | RigthS | T deriving Eq
data Falling = Enable | Disable deriving Eq
data Square = (Color,Falling) deriving Eq

-- pronta
findIndexes :: Int -> [[Square]] -> [(Int, Int)]
findIndexes s matrix = do
-- iterate by the rows and columns to get the i and j of the elements
  (i, row) <- zip [0..] matrix
  (j, elem) <- zip [0..] row
  if elem == s then return (i, j) else []

-- -- FALTA MODULARIZAR PARA OS LADOS
-- A DISCUTIR
-- canMove :: [[Square]] -> Bool
-- canMove matrix coordinate = if bellow == 2 then False else True
--     where bellow = matrix !! ((fst coordinate) + 1) !! (snd coordinate) 

--recebe uma grid e uma direção (esq, baixo, dir). retorna se os blocos ativos podem se mover nessa direçao
canMove :: [[Square]] -> Move -> Bool

-- PARA CONSULTA
--canFallDownBlock :: [[Int]] -> [(Int,Int)] -> Bool
--canFallDownBlock matrix [x] =  canFallDown matrix (fst x, snd x)
--canFallDownBlock matrix (x:xs) =
--    if canFallDown matrix (fst x, snd x) then  canFallDownBlock matrix xs
--    else False
-- pega a grid, o conjunto de coordenadas ativas e uma direção. retorna se elas podem ir pra uma direçao
canMoveTetromino :: [[Square]] -> [(Int,Int)] -> Int -> Bool


-- pronta
-- TROCAR PELA MOVE TETROMINO!!
fallTetromino :: [[Square]] -> [(Int,Int)] -> [[Square]]
fallTetromino :: [[Square]] -> [(Int,Int)] -> [[Square]]
fallTetromino matrix [x] = [((fst x + 1), snd x)]
fallTetromino matrix (x:xs) = ((fst x + 1), snd x) : fallTetromino matrix xs

--move todos os blocos ativos numa direção. presume que eles podem ser movidos
moveTetromino :: [[Square]] -> Move -> [[Square]]

-- S
-- Pega uma matriz e retorna a lista de linhas que podem ser limpas
clearableLines :: [[Square]] -> [Int]
-- usa canClearLine

--S
-- pega uma matriz e um índice. retorna se essa linha é clearável ou não
canClearLine :: [[Square]] -> Int -> Bool

--S
-- pega uma matriz e uma lista de índices. retorna uma matriz com todos esses índices clearados
clearTheseLines :: [[Square]] -> [Int] -> [[Square]]

-- S
-- pega uma matriz e retorna ela com todas as linhas clearáveis clearadas
clearMatrix :: [[Square]] -> ([[Square]], Int)
-- clearMatrix grid = 
    -- x = clearableLines grid
    -- clearedMatrix = clearTheseLines grid x
    -- return clearedMatrix 

-- L
-- pega uma matriz e um sentido. retorna essa matriz com os blocos ativos rotacionados pra direita
rotate :: [[Square]] -> [[Square]]

-- L
--pega as coordenadas de cada bloco de um tetromino. retorna as coordenadas finais que cada bloco deve ter.
getRotationEndPositions :: [(Int,Int)] -> [(Int,Int)]

-- E
--retorna se um conjunto de blocos pode ser colocado na grid.
canBePut :: [[Square]] -> [(Int, Int)] -> Bool

-- 
--retorna 1 se pode ser posto com um movimento pra esquerda. 2 com um pra direita. 0 se não pode
canBePutWithSideMove :: [[Square]] -> [(Int, Int)] -> Int

--pega um conjunto de coordenadas. sobe elas até que possa botar elas na matriz, então bota elas na matriz
raiseUntilAllowed :: [[Square]] -> [(Int, Int)] -> [[Square]]

--retorna true se tem algum bloco acima do limite da grid, false caso contrário
isGameOver :: [[Square]] -> Bool

--usa magia de Gloss para exibir a grid na tela
showGrid :: [[Square]] -> ????


showGameOver :: ????


fullFall :: [[Square]] -> [[Square]]

-- retorna a nova grid, com os blocos ativos derrubados pra baixo.
forceFall :: [[Square]]
--  showGrid grid
--  if isGameOver
--     showGameOver
--  if !canFall
--      goToNextCycle

-- desativa todos os blocos
groundBlocks :: [[Square]] -> [[Square]]

getRandomTetromino :: ([(Int, Int)], Int)

-- bota um tetromino aleatório na grid.
putRandomTetromino :: [[Square]] -> [[Square]]