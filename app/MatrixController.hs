module MatrixController where

import Data.List (minimumBy)
import System.Random (randomRIO)
import Data.Typeable (typeOf)
import System.IO.Unsafe (unsafePerformIO)
  
data Move = MoveNone | MoveLeft | MoveRight | MoveRotate | MoveDown | SuperDown | MoveSwap deriving Eq
data BlockColor = Black | Blue | Cyan | Orange | Yellow | Green | Violet | Red deriving (Eq, Show)
data Active = Prediction | Enable | Disable | None deriving Eq
data Square = Square BlockColor Active deriving Eq
data Tetromino = NullTetromino | Tetromino [(Int,Int)] BlockColor deriving Eq

getColor :: Square -> BlockColor
getColor (Square color _) = color

getCoordsFromTetromino :: Tetromino -> [(Int,Int)]
getCoordsFromTetromino (Tetromino coords _) = coords
getCoordsFromTetromino NullTetromino = []

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



getActive :: Square -> Active
getActive (Square _ active) = active

getActiveColor :: [[Square]] -> BlockColor
getActiveColor matrix = firstActiveColor
  where firstActive = if length (findActiveIndexes matrix) > 0 then head (findActiveIndexes matrix) else (-1, -1)
        firstActiveColor = if firstActive == (-1, -1) then Black else getColor (matrix !! (snd firstActive) !! (fst firstActive))

emptyLine :: [Square]
emptyLine = replicate 10 emptySquare
    where emptySquare = Square Black None
  
fullLine :: [Square]
fullLine = replicate 10 (Square Yellow Disable)

emptyMatrix :: [[Square]]
emptyMatrix = replicate 25 emptyLine

fullBaseMatrix :: [[Square]]
fullBaseMatrix = fullLine : (replicate 24 emptyLine)

--uses emptyMatrix
bottomLeftFilledMatrix :: [[Square]]
bottomLeftFilledMatrix = updateMatrixElement emptyMatrix (0,0) (Square Cyan Enable)

topLeftFilledMatrix :: [[Square]]
topLeftFilledMatrix = updateMatrixElement emptyMatrix (2,19) (Square Cyan Enable)

almostFullBaseMatrix :: [[Square]]
almostFullBaseMatrix = updateMatrixElement (updateMatrixElement fullBaseMatrix (0,0) (Square Black None)) (0,0) (Square Cyan Enable)

getTetrominoBlocks :: Tetromino -> [(Int, Int)]
getTetrominoBlocks (Tetromino blocksPos _) = blocksPos

getTetrominoColor :: Tetromino -> BlockColor
getTetrominoColor (Tetromino _ color) = color

-- TO TEST
findActiveIndexes :: [[Square]] -> [(Int, Int)]
findActiveIndexes matrix = do
-- iterate by the rows and columns to get the i and j of the elements
  (i, row) <- zip [0..] matrix
  (j, elem) <- zip [0..] row
  if (getActive elem) == Enable then return (j, i) else []

------------ PIECE PERMISSION LOGIC ------------

-- TO TEST
--retorna se um conjunto de blocos pode ser colocado na matrix.
canBePut :: [[Square]] -> [(Int, Int)] -> Bool
canBePut matrix [] = True
canBePut matrix (h : ts)
  | fst h < 0 || fst h > 9 || snd h < 0 || snd h > 24 = False
  | getActive(matrix !! (snd h) !! (fst h)) == Disable = False
  | otherwise = canBePut matrix ts

-- TO TEST
canMoveTetromino :: [[Square]] -> Move -> Bool
canMoveTetromino matrix move = canBePut matrix (getEndPos matrix move)




------------ PIECE MOVEMENT LOGIC ------------


-- magia.
updateMatrixElement :: [[Square]] -> (Int, Int) -> Square -> [[Square]]
updateMatrixElement matrix (j, i) newValue =
  take i matrix ++
  [take j (matrix !! i) ++ [newValue] ++ drop (j + 1) (matrix !! i)] ++
  drop (i + 1) matrix

-- TO TEST
-- Retorna a matrix sem blocos ativos
removeActiveBlocks :: [[Square]] -> [[Square]]
removeActiveBlocks [] = []
removeActiveBlocks (x:xs) =
    map (\(Square color active) -> if active == Enable then Square Black None else Square color active) x : removeActiveBlocks xs

removePrediction :: [[Square]] -> [[Square]]
removePrediction [] = []
removePrediction (x:xs) =
    map (\(Square color active) -> if active == Prediction then Square Black None else Square color active) x : removePrediction xs

-- TO TEST
addBlocks :: [[Square]] -> Square -> [(Int, Int)] -> [[Square]]
addBlocks matrix square [] = matrix
addBlocks matrix square (x:xs) = addBlocks updatedMatrix square xs
    where updatedMatrix = if getActive (matrix !! snd x !! fst x) /= Enable then updateMatrixElement matrix ((fst x), (snd x)) square else matrix

-- TO TEST
getEndPos :: [[Square]] -> Move -> [(Int, Int)]
getEndPos matrix move 
  | move == MoveLeft = map (\k -> ((fst k)-1, snd k)) (findActiveIndexes matrix)
  | move == MoveRight = map (\k -> ((fst k)+1, snd k)) (findActiveIndexes matrix)
  | move == MoveDown = map (\k -> (fst k, (snd k)-1)) (findActiveIndexes matrix)

-- TO TEST
-- remove todos os blocos ativos. bota blocos ativos nas posiçoes indicadas
changeActiveBlocksPos :: [[Square]] -> [(Int, Int)] -> [[Square]]
changeActiveBlocksPos matrix coordinates = addBlocks updatedMatrix square coordinates
    where 
        updatedMatrix = removeActiveBlocks matrix
        square = Square (getActiveColor matrix) Enable

-- TO TEST
-- matrix. 
moveTetromino :: [[Square]] -> Move -> [[Square]]
moveTetromino matrix move = changeActiveBlocksPos matrix endPos
    where endPos = getEndPos matrix move

getPredictionCoords :: [[Square]] -> [(Int, Int)]
getPredictionCoords matrix = 
    if canMoveTetromino matrix MoveDown then getPredictionCoords (moveTetromino matrix MoveDown)
    else findActiveIndexes matrix

updatePrediction :: [[Square]] -> [[Square]]
updatePrediction matrix = addBlocks (removePrediction matrix) (Square (getActiveColor matrix) Prediction) (getPredictionCoords matrix)

-- TO TEST
fullFall :: [[Square]] -> [[Square]]
fullFall matrix = 
    if canMoveTetromino matrix MoveDown then fullFall (moveTetromino matrix MoveDown)
    else matrix


------------ CLEAR MATRIX LOGIC ------------

-- TO TEST
-- pega uma matriz e um índice. retorna se essa linha é clearável ou não
canClearLine :: [Square] -> Bool
canClearLine line = all (\k -> getActive k /= None) line

-- TO TEST
-- pega uma matriz e uma lista de índices. retorna uma matriz com todos esses índices clearados
clearMatrix :: [[Square]] -> [[Square]]
clearMatrix matrix = (unclearable ++ replicate (clearableCount matrix) emptyLine)
  where unclearable = filter (\k -> not (canClearLine k)) matrix

-- TO TEST
clearableCount :: [[Square]] -> Int
clearableCount matrix = length (filter (\k -> canClearLine k) matrix)

-- TO TEST
goToNextCycle :: [[Square]] -> Int -> [[Square]]
goToNextCycle matrix seed = putRandomTetromino (clearMatrix (groundBlocks matrix)) seed


------------ GAME LOGIC ------------


-- TO TEST
--retorna true se tem algum bloco acima do limite da matrix, false caso contrário
isGameOver :: [[Square]] -> Bool
isGameOver matrix = not (all (\k -> getColor k == Black) (concat matrixTop))
    where matrixTop = drop 20 matrix

-- TO TEST
-- for every Square in the matrix, set the active to Disable if it is Enable
groundBlocks :: [[Square]] -> [[Square]]
groundBlocks [] = []
groundBlocks (x:xs) = map (\(Square color active) -> if active == Enable then Square color Disable else Square color active) x : groundBlocks xs

getRandomTetromino :: Int -> Tetromino
getRandomTetromino seed = xs !! idx
  where
    xs = [Tetromino [(0,0),(1,0),(2,0),(3,0)] Cyan, Tetromino [(0,0),(1,0),(0,-1),(0,-2)] Orange, Tetromino [(0,0),(1,0),(0,-1),(1,-1)] Yellow, Tetromino [(0,0),(1,0),(1,-1),(1,-2)] Green, Tetromino [(0,0),(1,0),(2,0),(1,-1)] Violet, Tetromino [(0,0),(1,0),(1,-1),(2,-1)] Blue, Tetromino [(0,0),(0,-1),(1,-1),(1,-2)] Red]
    idx = ((unsafePerformIO $ randomRIO (0, length xs - 1)) + seed) `mod` 7
-- P
--getRandomTetromino :: IO Tetromino
--getRandomTetromino = case randomRIO (0, 6) of
--    0 -> Tetromino [(0,0),(1,0),(2,0),(3,0)] Cyan -- I
--    1 -> Tetromino [(0,0),(1,0),(0,1),(0,2)] Orange -- L
--    2 -> Tetromino [(0,0),(1,0),(0,1),(1,1)] Yellow -- O
--    3 -> Tetromino [(0,0),(1,0),(1,1),(1,2)] Green -- S
--    4 -> Tetromino [(0,0),(1,0),(2,0),(1,1)] Violet -- T
 --   5 -> Tetromino [(0,0),(1,0),(1,1),(2,1)] Blue -- J
--    6 -> Tetromino [(0,0),(0,1),(1,1),(1,2)] Red -- Z


-- P
-- bota um tetromino aleatório na matrix.

-- Aqui embaixo acho q vai haver um problema com a estrutura do codigo no geral, eu n acho
-- q a parte q é responsavel por chamar esse comando dnv deve estar aqui dentro, ja que ele (presumo)
-- n é responsavel por checar se a peça ja foi encaixada. A discutir
-- PS eu provavelmente me atrapalhei com os numeros mas isso pode ser concertado rapidamente.
-- So mandar msg.


-- TO TEST
putRandomTetromino :: [[Square]] -> Int -> [[Square]]
putRandomTetromino matrix seed = addBlocks matrix (Square (getTetrominoColor newTetronimo) Enable) (raiseUntilAllowed matrix (map (\k -> addTuples k (3, 19)) (getTetrominoBlocks newTetronimo)))
  where newTetronimo = getRandomTetromino seed

swapTetromino :: [[Square]] -> Tetromino -> [[Square]]
swapTetromino matrix tetromino = addBlocks clearedMatrix (Square (getTetrominoColor tetromino) Enable) finalCoords
  where
    currentBaseDist = baseDistance (findActiveIndexes matrix)
    tetrominoBaseDist = baseDistance (getCoordsFromTetromino tetromino)
    clearedMatrix = removeActiveBlocks matrix
    finalCoords = raiseUntilAllowed clearedMatrix (map (\k -> addTuples k (subtractTuples currentBaseDist tetrominoBaseDist)) (getCoordsFromTetromino tetromino))


------------ PIECE ROTATION LOGIC ------------

-- TO TEST
canBePutWithSideMove :: [[Square]] -> [(Int, Int)] -> Int
canBePutWithSideMove matrix positions
    | canBePut matrix (map (\k -> ((fst k) - 1, snd k)) positions) = 1
    | canBePut matrix (map (\k -> ((fst k) + 1, snd k)) positions) = 2
    | otherwise = 0

-- TO TEST
-- pega uma matriz e um sentido. retorna essa matriz com os blocos ativos rotacionados pra direita
-- TO TEST
-- pega uma matriz e um sentido. retorna essa matriz com os blocos ativos rotacionados pra direita
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

bringIndexesToZeroZero :: [(Int, Int)] -> [(Int, Int)]
bringIndexesToZeroZero indexes = zeroedIndexes
  where
    baseDist = baseDistance indexes
    zeroedIndexes = map (\x -> subtractTuples x baseDist) indexes




-- TO TEST
-- pega um conjunto de pontos na matrix. retorna, o ponto no top left
baseDistance :: [(Int, Int)] -> (Int, Int)
baseDistance coords = (minimum (map (\x -> fst x) coords), maximum (map (\x -> snd x) coords))


-- TO TEST
subtractTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
subtractTuples (a, b) (c, d) = (a - c, b - d)


-- TO TEST
addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (a, b) (c, d) = (a + c, b + d)


-- TO TEST
-- rotaciona um conjunto de pontos no sentido horário
rotatePoints :: [(Int, Int)] -> [(Int, Int)]
rotatePoints cloud = map (\k -> ((fst k) - minX, (snd k) - maxY)) rotated
  where
    rotated = map (\x -> (snd x, -(fst x))) cloud
    minX = minimum (map (\x -> fst x) rotated)
    maxY = maximum (map (\x -> snd x) rotated)


-- TO TEST
--pega um conjunto de coordenadas. sobe elas até que possa botar elas na matriz, então retorna as novas coordenadas
raiseUntilAllowed :: [[Square]] -> [(Int, Int)] -> [(Int, Int)]
raiseUntilAllowed matrix coords 
  | canBePut matrix coords = coords
  | canBePutWithSideMove matrix coords == 1 = map (\k -> (((fst k) - 1), (snd k))) coords
  | canBePutWithSideMove matrix coords == 2 = map (\k -> (((fst k) + 1), (snd k))) coords
  | otherwise = raiseUntilAllowed matrix (map (\k -> ((fst k), ((snd k) + 1))) coords)

encloseCoords :: [(Int, Int)] -> [(Int, Int)]
encloseCoords coords = newCoords
  where
    minX = minimum (map (\k -> fst k) coords)
    maxX = maximum (map (\k -> fst k) coords)
    newCoords
      | minX < 0 = map (\k -> ((fst k) - minX, snd k)) coords
      | maxX >= 10 = map (\k -> ((fst k) - (maxX - 9), snd k)) coords
      | otherwise = coords

-- getRandomElement :: [a] -> Maybe a
-- getRandomElement [] = Nothing
-- getRandomElement xs = Just $ xs !! index
--   where
--     (lo, hi) = (0, length xs - 1)
--     index = fst $ randomR (lo, hi) (mkStdGen 42) -- Using a fixed seed for reproducibility