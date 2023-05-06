module MatrixController where
  
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

getPosMove :: [[Square]] -> Move -> [(Int, Int)]
getPosMove matrix move 
  | move == Left = map (\k -> ((fst k)-1, snd k)) (findActiveIndexes grid)
  | move == Right = map (\k -> ((fst k)+1, snd k)) (findActiveIndexes grid)
  | move == Down = map (\k -> (fst k, (snd k)+1)) (findActiveIndexes grid)

-- TO TEST
--recebe uma matrix e uma direção (esq, baixo, dir). retorna se o bloco de uma coordenada pode se mover nessa direçao
canMove :: [[Square]] -> (Int, Int) -> Move -> Bool
canMove matrix coordinate move = if (snd posMove) == Disable then False else True
    where posMove =
        if move == Left then matrix !! (fst coordinate) !! ((snd coordinate) - 1)
        else if move == Right then matrix !! (fst coordinate) !! ((snd coordinate) + 1)
        else if move == Down then matrix !! ((fst coordinate) + 1) !! (snd coordinate)
 
-- TO TEST
canMoveTetromino :: [[Square]] -> Move -> Boll
canMoveTetromino matrix move = length (filter (==False) (map (\k -> canMove matrix k move) findActiveIndexes)) == 0


-- magia.
updateMatrixElement :: [[Square]] -> (Int, Int) -> Square -> [[Square]]
updateMatrixElement matrix (i, j) newValue =
  take i matrix ++
  [take j (matrix !! i) ++ [newValue] ++ drop (j + 1) (matrix !! i)] ++
  drop (i + 1) matrix


-- S
-- Retorna a matrix sem blocos ativos
removeActiveBlocks :: [[Square]] -> [[Square]]
removeActiveBlocks [] = []
removeActiveBlocks (x:xs) =
    map ((Square Color Active) -> if Active == Enable then Square Black None else Square Color Active) x : removeActiveBlocks xs

addBlocks :: [[Square]] -> Square -> [(Int, Int)] -> [[Square]]
addBlocks matrix square [] = [] 
addBlocks matrix square (x:xs) = addBlocks updatedMatrix square xs
    where updatedMatrix = updateMatrixElement matrix ( fst x, snd x) square


-- remove todos os blocos ativos. bota blocos ativos nas posiçoes indicadas
changeActiveBlocksPos :: [[Square]] -> Move -> [(Int, Int)]
changeActiveBlocksPos matrix move = 

-- matrix. 
moveTetromino :: [[Square]] -> Move -> [[Square]]
moveTetromino matrix [x] move = moveBlock matrix (fst x, snd x) move 
moveTetromino matrix (x:xs) move =posMove : moveTetromino matrix xs move
    where posMove =
        if move == Left then ((fst x + 1), snd x)
        else if move == Right then (fst x, (snd x + 1))
        else if move == Down then matrix !! ((fst x + 1), snd x)

-- moveTetromino
-- changeActiveBlocksPos indixes auxMatrix
-- where
--     indixes = findActiveIndexes
--     auxMatrix = removeActiveBlocks matrix

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

--S
-- pega uma matriz e uma lista de índices. retorna uma matriz com todos esses índices clearados
clearTheseLines :: [[Square]] -> [Int] -> [[Square]]
clearTheseLines matrix lines =


-- S
-- pega uma matriz e retorna ela com todas as linhas clearáveis clearadas
clearMatrix :: [[Square]] -> ([[Square]], Int)
-- clearMatrix matrix = 
    -- x = clearableLines matrix
    -- clearedMatrix = clearTheseLines matrix x
    -- return clearedMatrix 


getColor :: Square -> Color
getColor (Square color _) = color

-- S
--retorna se um conjunto de blocos pode ser colocado na matrix.
canBePut :: [[Square]] -> [(Int, Int)] -> Bool
-- S
--retorna 1 se pode ser posto com um movimento pra esquerda. 2 com um pra direita. 0 se não pode
canBePutWithSideMove :: [[Square]] -> [(Int, Int)] -> Int

-- S
--retorna true se tem algum bloco acima do limite da matrix, false caso contrário
isGameOver :: [[Square]] -> Bool


-- S
fullFall :: [[Square]] -> [[Square]]

-- S
-- retorna a nova matrix, com os blocos ativos derrubados pra baixo.
forceFall :: [[Square]]
--  showmatrix matrix
--  if isGameOver
--     showGameOver
--  if !canFall
--      goToNextCycle

-- S
-- desativa todos os blocos
groundBlocks :: [[Square]] -> [[Square]]

-- P
getRandomTetromino :: ([(Int, Int)], Int)


-- P
-- bota um tetromino aleatório na matrix.
putRandomTetromino :: [[Square]] -> [[Square]]


------------ PIECE ROTATION LOGIC ------------

-- TO TEST
-- pega uma matriz e um sentido. retorna essa matriz com os blocos ativos rotacionados pra direita
rotate :: [[Square]] -> [[Square]]
rotate matrix =
  let activeIndexes = findActiveIndexes matrix
  let baseDist = baseDistance matrix
  let zeroedIndexes = map (\x -> subtractTuples x baseDist) activeIndexes
  let rotatedZeroed = rotatePoints zeroedIndexes
  let returnedToPos = rotatePoints
rotate matrix = raiseUntilAllowed matrix returnedToPos

-- TO TEST
-- pega um conjunto de pontos na matrix. retorna, dentre os pontos mais baixos, o mais à esquerda
baseDistance :: [(Int, Int)] -> (Int, Int)
baseDistance cloud =
  let base = filter (\x -> snd x == min (map snd cloud))
      maisEsquerdaDaBase = filter (\x -> fst x == min (map fst base)) base
  in head maisEsquerdaDaBase


-- TO TEST
subtractTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
subtractTuples (a, b) (c, d) = (a - c, b - d)


-- TO TEST
addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (a, b) (c, d) = (a + c, b + d)


-- TO TEST
-- rotaciona um conjunto de pontos no sentido horário
rotatePoints :: [(Int, Int)] -> [(Int, Int)]
rotatePoints cloud = map (\x -> (snd x, -(fst x))) cloud


-- TO TEST
--pega um conjunto de coordenadas. sobe elas até que possa botar elas na matriz, então bota elas na matriz
raiseUntilAllowed :: [[Square]] -> [(Int, Int)] -> [[Square]]
raiseUntilAllowed matrix coords 
  | canBePut matrix coords = changeActiveBlocksPos matrix coords
  | canBePutWithSideMove matrix coords == 1 = changeActiveBlocksPos matrix (map (\k -> (((fst k) - 1), (snd k))) coords)
  | canBePutWithSideMove matrix coords == 2 = changeActiveBlocksPos matrix (map (\k -> (((fst k) + 1), (snd k))) coords)
  | otherwise = raiseUntilAllowed matrix (map (\k -> ((fst k), ((snd k) + 1))) coords)
