module MatrixController where

import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)

--datatypes
-- define os comandos de movimento
data Move = MoveNone | MoveLeft | MoveRight | MoveRotate | MoveDown | SuperDown | MoveSwap deriving Eq
-- define as cores que usamos no jogo
data BlockColor = Black | Blue | Cyan | Orange | Yellow | Green | Violet | Red deriving (Eq, Show)
-- define o tipo do Square
data Active = Prediction | Enable | Disable | None deriving Eq
-- define os quadrados que compoem a matriz
data Square = Square BlockColor Active deriving Eq
-- define as peças
data Tetromino = NullTetromino | Tetromino [(Int,Int)] BlockColor deriving Eq

-- retorna a cor de um quadrado
getColor :: Square -> BlockColor
getColor (Square color _) = color

-- função para gerar números aleatorios (logica usada sempre que aparece uma nova peça)
veryRandom :: [Square] -> Int -> Int
veryRandom [] _ = 0
veryRandom (x:xs) index = index * (colority + activity) + veryRandom xs (index + 1)
  where 
    activity
      | getActive x == Enable = 1
      | getActive x == Disable = 4
      | otherwise = 9
    colority
      | getColor x == Black = 17
      | getColor x == Blue = 2
      | getColor x == Cyan = 31
      | getColor x == Orange = 48
      | getColor x == Yellow = 53
      | getColor x == Green = 62
      | getColor x == Violet = 71
      | getColor x == Red = 88
      | otherwise = 1


-- retorna o estado de um quadrado
getActive :: Square -> Active
getActive (Square _ active) = active

-- retorna a cor dos quadrados que estão caindo
getActiveColor :: [[Square]] -> BlockColor
getActiveColor matrix = firstActiveColor
  where firstActive = if length (findActiveIndexes matrix) > 0 then head (findActiveIndexes matrix) else (-1, -1)
        firstActiveColor = if firstActive == (-1, -1) then Black else getColor (matrix !! (snd firstActive) !! (fst firstActive))

-- retorna uma linha vazia
emptyLine :: [Square]
emptyLine = replicate 10 emptySquare
    where emptySquare = Square (Color Black) (Active None)

-- retorna uma matriz vazia
emptyMatrix :: [[Square]]
emptyMatrix = replicate 25 emptyLine

-- retorna as coordenadas de um tetromino
getTetrominoBlocks :: Tetromino -> [(Int, Int)]
getTetrominoBlocks (Tetromino blocksPos _) = blocksPos
getTetrominoBlocks NullTetromino = []

-- retorna a cor de um tetromino
getTetrominoColor :: Tetromino -> BlockColor
getTetrominoColor (Tetromino _ color) = color
getTetrominoColor NullTetromino = Black

-- retorna as coordenadas onde existem blocos caindo
findActiveIndexes :: [[Square]] -> [(Int, Int)]
findActiveIndexes matrix = do
  (i, row) <- zip [0..] matrix
  (j, elem) <- zip [0..] row
  if (getActive elem) == Enable then return (j, i) else []

------------ PIECE PERMISSION LOGIC ------------


--retorna se um conjunto de blocos pode ser colocado na matrix.
canBePut :: [[Square]] -> [(Int, Int)] -> Bool
canBePut _ [] = True
canBePut matrix (h : ts)
  | fst h < 0 || fst h > 9 || snd h < 0 || snd h > 24 = False
  | getActive(matrix !! (snd h) !! (fst h)) == Disable = False
  | otherwise = canBePut matrix ts

-- retorna se é possível mover o tetromino para certa posição
canMoveTetromino :: [[Square]] -> Move -> Bool
canMoveTetromino matrix move = canBePut matrix (getEndPos matrix move)




------------ PIECE MOVEMENT LOGIC ------------


-- atualiza um elemento da matriz (magia!).
updateMatrixElement :: [[Square]] -> (Int, Int) -> Square -> [[Square]]
updateMatrixElement matrix (j, i) newValue =
  take i matrix ++
  [take j (matrix !! i) ++ [newValue] ++ drop (j + 1) (matrix !! i)] ++
  drop (i + 1) matrix

-- retorna a matriz sem blocos ativos
removeActiveBlocks :: [[Square]] -> [[Square]]
removeActiveBlocks [] = []
removeActiveBlocks (x:xs) =
    map ((Square Color Active) -> if Active == Enable then Square Black None else Square Color Active) x : removeActiveBlocks xs

-- retorna a matriz sem blocos de previsão
removePrediction :: [[Square]] -> [[Square]]
removePrediction [] = []
removePrediction (x:xs) =
    map (\(Square color active) -> if active == Prediction then Square Black None else Square color active) x : removePrediction xs

-- adiciona o quadrado do parametro nas coordenadas também passadas como parametro
addBlocks :: [[Square]] -> Square -> [(Int, Int)] -> [[Square]]
addBlocks matrix _ [] = matrix
addBlocks matrix square (x:xs) = addBlocks updatedMatrix square xs
    where updatedMatrix = if getActive (matrix !! snd x !! fst x) /= Enable then updateMatrixElement matrix ((fst x), (snd x)) square else matrix

-- retorna as coordenadas finais de um movimento
getEndPos :: [[Square]] -> Move -> [(Int, Int)]
getEndPos matrix move 
  | move == MoveLeft = map (\k -> ((fst k)-1, snd k)) (findActiveIndexes matrix)
  | move == MoveRight = map (\k -> ((fst k)+1, snd k)) (findActiveIndexes matrix)
  | move == MoveDown = map (\k -> (fst k, (snd k)-1)) (findActiveIndexes matrix)
  | otherwise = (findActiveIndexes matrix)

-- remove todos os blocos ativos. bota blocos ativos nas posiçoes indicadas
changeActiveBlocksPos :: [[Square]] -> [(Int, Int)] -> [[Square]]
changeActiveBlocksPos matrix coordinates = addBlocks updatedMatrix square coordinates
    where 
        updatedMatrix = removeActiveBlocks matrix
        square = Square (getActiveColor matrix) (Active Enable)

-- movimenta o Tetromino
moveTetromino :: [[Square]] -> Move -> [[Square]]
moveTetromino matrix move = changeActiveBlocksPos matrix endPos
    where endPos = getEndPos matrix move

-- pega as coordenadas dos blocos de previsão
getPredictionCoords :: [[Square]] -> [(Int, Int)]
getPredictionCoords matrix = 
    if canMoveTetromino matrix MoveDown then getPredictionCoords (moveTetromino matrix MoveDown)
    else findActiveIndexes matrix

-- atualiza a previsão de acordo com a nova matriz
updatePrediction :: [[Square]] -> [[Square]]
updatePrediction matrix = addBlocks (removePrediction matrix) (Square (getActiveColor matrix) Prediction) (getPredictionCoords matrix)

-- derruba a peça instantaneamente
fullFall :: [[Square]] -> [[Square]]
fullFall matrix = 
    if canMoveTetromino matrix (Move MoveDown) then fullFall (moveTetromino matrix (Move MoveDown))
    else matrix


------------ CLEAR MATRIX LOGIC ------------

-- pega uma matriz e um índice. retorna se essa linha é clearável ou não
canClearLine :: [Square] -> Bool
canClearLine line = all (\k -> getActive k /= None) line

-- pega uma matriz e uma lista de índices. retorna uma matriz com todos esses índices clearados
clearMatrix :: [[Square]] -> ([[Square]], Int)
clearTheseLines matrix lines = ((remainderLines ++ replicate (25 - length remainderLines) emptyLine), (25 - length remainderLines))
    where remainderLines = filter (\k -> canClearLine k) matrix


-- retorna a quantidade de linhas que podem ser limpas de uma vez
clearableCount :: [[Square]] -> Int
clearableCount matrix = length (filter (\k -> canClearLine k) matrix)

-- reinicia o ciclo chamando uma nova peça e limpando as linhas completas
goToNextCycle :: [[Square]] -> Int -> [[Square]]
goToNextCycle matrix seed = putRandomTetromino (clearMatrix (groundBlocks matrix)) seed


------------ GAME LOGIC ------------

--retorna true se tem algum bloco acima do limite da matriz, false caso contrário
isGameOver :: [[Square]] -> Bool
isGameOver matrix = not (all (\k -> getColor k == (Color Black)) (concat matrixTop))
    where matrixTop = drop 20 matrix

-- para cada quadrado da matriz, desativa caso seja ativo
groundBlocks :: [[Square]] -> [[Square]]
groundBlocks [] = []
groundBlocks (x:xs) = map (\Square color active ->Square color (if active == (Active Enable) then (Active Disable) else active)) x ++ groundBlocks xs

-- gera uma peça aleatória
getRandomTetromino :: Int -> Tetromino
getRandomTetromino seed = xs !! idx
  where
    xs = [Tetromino [(0,0),(1,0),(2,0),(3,0)] Cyan, Tetromino [(0,0),(1,0),(0,-1),(0,-2)] Orange, Tetromino [(0,0),(1,0),(0,-1),(1,-1)] Yellow, Tetromino [(0,0),(1,0),(1,-1),(1,-2)] Green, Tetromino [(0,0),(1,0),(2,0),(1,-1)] Violet, Tetromino [(0,0),(1,0),(1,-1),(2,-1)] Blue, Tetromino [(0,0),(0,-1),(1,-1),(1,-2)] Red]
    idx = ((unsafePerformIO $ randomRIO (0, length xs - 1)) + seed) `mod` 7



-- coloca em jogo uma peça aleatória
putRandomTetromino :: [[Square]] -> Int -> [[Square]]
putRandomTetromino matrix seed = addBlocks matrix (Square (getTetrominoColor newTetronimo) Enable) (raiseUntilAllowed matrix (map (\k -> addTuples k (3, 19)) (getTetrominoBlocks newTetronimo)))
  where newTetronimo = getRandomTetromino seed

-- troca a peça atual por uma reserva
swapTetromino :: [[Square]] -> Tetromino -> [[Square]]
swapTetromino matrix tetromino = addBlocks clearedMatrix (Square (getTetrominoColor tetromino) Enable) finalCoords
  where
    currentBaseDist = baseDistance (findActiveIndexes matrix)
    tetrominoBaseDist = baseDistance (getTetrominoBlocks tetromino)
    clearedMatrix = removeActiveBlocks matrix
    finalCoords = raiseUntilAllowed clearedMatrix (map (\k -> addTuples k (subtractTuples currentBaseDist tetrominoBaseDist)) (getTetrominoBlocks tetromino))


------------ PIECE ROTATION LOGIC ------------

-- verifica se a peça pode ser colocada apenas com um movimento lateral
canBePutWithSideMove :: [[Square]] -> [(Int, Int)] -> Int
canBePutWithSideMove matrix positions
    | canBePut matrix (map (\k -> ((fst k) - 1, snd k)) positions) = 1
    | canBePut matrix (map (\k -> ((fst k) + 1, snd k)) positions) = 2
    | otherwise = 0

-- pega uma matriz. retorna essa matriz com os blocos ativos rotacionados pra direita
rotateTetromino :: [[Square]] -> [[Square]]
rotateTetromino matrix =
  let activeIndexes = findActiveIndexes matrix
      baseDist = baseDistance (findActiveIndexes matrix)
      zeroedIndexes = map (\x -> subtractTuples x baseDist) activeIndexes
      rotatedZeroed = rotatePoints zeroedIndexes
      returnedToPos = map (\x -> addTuples x baseDist) rotatedZeroed
      enclosed = encloseCoords returnedToPos
      finalPos = raiseUntilAllowed matrix enclosed
    in changeActiveBlocksPos matrix finalPos

-- deixa o ponto superior esquerdo de um conjunto de coordenadas zerado. todas as outras relações se mantém
bringIndexesToZeroZero :: [(Int, Int)] -> [(Int, Int)]
bringIndexesToZeroZero indexes = zeroedIndexes
  where
    baseDist = baseDistance indexes
    zeroedIndexes = map (\x -> subtractTuples x baseDist) indexes

-- pega um conjunto de pontos na matriz. retorna, o ponto no top left
baseDistance :: [(Int, Int)] -> (Int, Int)
baseDistance coords = (minimum (map (\x -> fst x) coords), maximum (map (\x -> snd x) coords))


-- subtrai tuplas
subtractTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
subtractTuples (a, b) (c, d) = (a - c, b - d)


-- soma tuplas
addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (a, b) (c, d) = (a + c, b + d)


-- rotaciona um conjunto de pontos no sentido horário
rotatePoints :: [(Int, Int)] -> [(Int, Int)]
rotatePoints cloud = map (\k -> ((fst k) - minX, (snd k) - maxY)) rotated
  where
    rotated = map (\x -> (snd x, -(fst x))) cloud
    minX = minimum (map (\x -> fst x) rotated)
    maxY = maximum (map (\x -> snd x) rotated)


--pega um conjunto de coordenadas. sobe elas até que possa botar elas na matriz, então retorna as novas coordenadas
raiseUntilAllowed :: [[Square]] -> [(Int, Int)] -> [(Int, Int)]
raiseUntilAllowed matrix coords 
  | canBePut matrix coords = coords
  | canBePutWithSideMove matrix coords == 1 = map (\k -> (((fst k) - 1), (snd k))) coords
  | canBePutWithSideMove matrix coords == 2 = map (\k -> (((fst k) + 1), (snd k))) coords
  | otherwise = raiseUntilAllowed matrix (map (\k -> ((fst k), ((snd k) + 1))) coords)

-- coloca coordenadas dentro dos limites da matriz
encloseCoords :: [(Int, Int)] -> [(Int, Int)]
encloseCoords coords = newCoords
  where
    minX = min (map (\k -> fst k) coords)
    maxX = max (map (\k -> fst k) coords)
    newCoords
      | minX < 0 = map (\k -> ((fst k) - minX, snd k)) coords
      | maxX >= 10 = map (\k -> ((fst k) - (maxX - 9), snd k)) coords
      | otherwise = coords
