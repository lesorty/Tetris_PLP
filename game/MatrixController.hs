data Move = Left | Right | Rotate | Down | SuperDown
data Color = Black | Blue | Cyan | Orange | Yellow | Green | Violet | Red deriving Eq
data Piece = LeftL | RightL | Square | Rectangule | LeftS | RigthS | T deriving Eq
data Active = Enable | Disable | None deriving Eq
data Square = (Color,Active) deriving Eq

-- TO TEST
findActiveIndexes :: [[Square]] -> [(Int, Int)]
findActiveIndexes s matrix = do
-- iterate by the rows and columns to get the i and j of the elements
  (i, row) <- zip [0..] matrix
  (j, elem) <- zip [0..] row
  if elem == Enable then return (i, j) else []


-- TO TEST
--recebe uma grid e uma direção (esq, baixo, dir). retorna se os blocos ativos podem se mover nessa direçao
canMove :: [[Square]] -> (Int, Int) -> Move -> Bool
canMove matrix coordinate move = if (snd posMove) == Disable then False else True
    where posMove =
        if move == Left then matrix !! (fst coordinate) !! ((snd coordinate) - 1)
        else if move == Right then matrix !! (fst coordinate) !! ((snd coordinate) + 1)
        else if move == Down then matrix !! ((fst coordinate) + 1) !! (snd coordinate)

-- TO TEST
-- pega a grid, o conjunto de coordenadas ativas e uma direção. retorna se elas podem ir pra uma direçao
canMoveTetromino :: [[Square]] -> [(Int,Int)] -> Move -> Bool
canMoveTetromino matrix [x] move = canMove matrix (fst x, snd x) move
canMoveTetromino matrix (x:xs) move = 
    if canMove matrix (fst x, snd x) move then canMoveTetromino matrix xs move
    else False

-- magia.
updateMatrixElement :: [[Square]] -> (Int, Int) -> Square -> [[Square]]
updateMatrixElement matrix (i, j) newValue =
  take i matrix ++
  [take j (matrix !! i) ++ [newValue] ++ drop (j + 1) (matrix !! i)] ++
  drop (i + 1) matrix


-- recebe uma posição na matrix e move para a direção desejada
moveBlock :: [[Square]] -> (Int, Int) -> Move -> [[Square]]
moveBlock matrix coordinate move =
    updateMatrixElement auxMatrix coordinate (Black, None) 
    where
        posMove =
            if move == Left then matrix !! (fst coordinate) !! ((snd coordinate) - 1)
            else if move == Right then matrix !! (fst coordinate) !! ((snd coordinate) + 1)
            else if move == Down then matrix !! ((fst coordinate) + 1) !! (snd coordinate)
        -- create an aux matrix that will have the element on the position after move but still have on the initial position to
        auxMatrix = updateMatrixElement matrix posMove (matrix !! fst coordinate !! snd coordinate)


moveTetromino :: [[Square]] -> [(Int, Int)] -> Move -> [[Square]]
moveTetromino matrix [x] move = moveBlock matrix (fst x, snd x) move 
moveTetromino matrix (x:xs) move =posMove : moveTetromino matrix xs move
    where posMove =
        if move == Left then ((fst x + 1), snd x)
        else if move == Right then (fst x, (snd x + 1))
        else if move == Down then matrix !! ((fst x + 1), snd x)




-- TO TEST
-- Pega uma matriz e retorna a lista de linhas que podem ser limpas
clearableLines :: [[Square]] -> [Int]
clearableLines matrix = [i | (row, i) <- zip matrix [0..], canClearLine matrix i]

-- usa canClearLine

-- TO TEST
-- pega uma matriz e um índice. retorna se essa linha é clearável ou não
canClearLine :: [[Square]] -> Int -> Bool
canClearLine matrix rowIndex = allDisable (matrix !! rowIndex)
    where allDisable xs = all (==Disable) xs

-- TODO
-- pega uma matriz e uma lista de índices. retorna uma matriz com todos esses índices clearados
clearTheseLines :: [[Square]] -> [Int] -> [[Square]]
clearTheseLines matrix lines =

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