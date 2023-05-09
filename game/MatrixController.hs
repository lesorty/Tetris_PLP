module MatrixController where
  
data Move = MoveNone | MoveLeft | MoveRight | MoveRotate | MoveDown | SuperDown deriving Eq
data BlockColor = Black | Blue | Cyan | Orange | Yellow | Green | Violet | Red deriving Eq
data Piece = LeftL | RightL | Square | Rectangule | LeftS | RigthS | T deriving Eq
data Active = Enable | Disable | None deriving Eq
data Square = Square BlockColor Falling deriving Eq
data Tetromino = Tetromino [(Int,Int)] BlockColor deriving Eq

getColor :: Square -> BlockColor
getColor (Square color _) = color

getActive :: Square -> Active
getActive (Square _ active) = active

getActiveColor :: [[Square]] -> BlockColor
getActiveColor matrix = 
  where firstActive = head (findActiveIndexes matrix)
        firstActiveColor = getColor (matrix !! (snd firstActive) !! (fst firstActive))

emptyMatrix :: [[Square]]
emptyMatrix = replicate 25 emptyLine

getTetrominoBlocks :: Tetromino -> [(Int, Int)]
getTetrominoBlocks (Tetromino blocksPos _) = blocksPos

getTetrominoColor :: Tetromino -> BlockColor
getTetrominoBlocks (Tetromino _ color) = color

-- TO TEST
findActiveIndexes :: [[Square]] -> [(Int, Int)]
findActiveIndexes s matrix = do
-- iterate by the rows and columns to get the i and j of the elements
  (i, row) <- zip [0..] matrix
  (j, elem) <- zip [0..] row
  if elem == Enable then return (i, j) else []

emptyLine :: [Square]
emptyLine = replicate 10 emptySquare
    where emptySquare = Square Black None

------------ PIECE PERMISSION LOGIC ------------

-- TO TEST
--retorna se um conjunto de blocos pode ser colocado na matrix.
canBePut :: [[Square]] -> [(Int, Int)] -> Bool
canBePut matrix [] = true
canBePut matrix (h : ts) = if getActive(matrix !! (fst h) !! (snd h)) == Disable then False else canBePut matrix ts

-- TO TEST
canMoveTetromino :: [[Square]] -> Move -> Bool
canMoveTetromino matrix move = canBePut matrix (getEndPos matrix move)




------------ PIECE MOVEMENT LOGIC ------------


-- magia.
updateMatrixElement :: [[Square]] -> (Int, Int) -> Square -> [[Square]]
updateMatrixElement matrix (i, j) newValue =
  take i matrix ++
  [take j (matrix !! i) ++ [newValue] ++ drop (j + 1) (matrix !! i)] ++
  drop (i + 1) matrix

-- TO TEST
-- Retorna a matrix sem blocos ativos
removeActiveBlocks :: [[Square]] -> [[Square]]
removeActiveBlocks [] = []
removeActiveBlocks (x:xs) =
    map ((Square color active) -> if active == Enable then Square Black None else Square color active) x : removeActiveBlocks xs

-- TO TEST
addBlocks :: [[Square]] -> Square -> [(Int, Int)] -> [[Square]]
addBlocks matrix square [] = [] 
addBlocks matrix square (x:xs) = addBlocks updatedMatrix square xs
    where updatedMatrix = updateMatrixElement matrix ((fst x), (snd x)) square

-- TO TEST
getEndPos :: [[Square]] -> Move -> [(Int, Int)]
getEndPos matrix move 
  | move == (Move MoveLeft) = map (\k -> ((fst k)-1, snd k)) (findActiveIndexes matrix)
  | move == (Move MoveRight) = map (\k -> ((fst k)+1, snd k)) (findActiveIndexes matrix)
  | move == (Move MoveDown) = map (\k -> (fst k, (snd k)+1)) (findActiveIndexes matrix)

-- TO TEST
-- remove todos os blocos ativos. bota blocos ativos nas posiçoes indicadas
changeActiveBlocksPos :: [[Square]] -> [(Int, Int)] -> [[Square]]
changeActiveBlocksPos matrix coordinates = addBlocks updatedMatrix square coordinates
    where 
        updatedMatrix = removeActiveBlocks matrix
        square = Square (getActiveColor matrix) (Active Enable)

-- TO TEST
-- matrix. 
moveTetromino :: [[Square]] -> Move -> [[Square]]
moveTetromino matrix move = changeActiveBlocksPos matrix endPos
    where endPos = getEndPos matrix move


-- TO TEST
fullFall :: [[Square]] -> [[Square]]
fullFall matrix = 
    if canMoveTetromino matrix (Move MoveDown) then fullFall (moveTetromino matrix (Move MoveDown))
    else matrix


-- L
-- retorna a nova matrix, com os blocos ativos derrubados pra baixo.
forceFall :: [[Square]]
--  showmatrix matrix
--  if isGameOver
--     showGameOver
--  if !canFall
--      goToNextCycle




------------ CLEAR MATRIX LOGIC ------------


-- TO TEST
-- pega uma matriz e um índice. retorna se essa linha é clearável ou não
canClearLine :: [Square] -> Bool
canClearLine line = all (\k -> getActive k == (Active Disable)) line

-- TO TEST
-- pega uma matriz e uma lista de índices. retorna uma matriz com todos esses índices clearados
clearMatrix :: [[Square]] -> ([[Square]], Int)
clearTheseLines matrix lines = ((clearable ++ replicate (25 - clearable) emptyLine), (25 - clearable))
where clearable = clearableCount matrix

-- TO TEST
clearableCount :: [[Square]] -> Int
clearableCount matrix = length (filter (\k -> canClearLine k) matrix)

-- TO TEST
goToNextCycle :: [[Square]] -> [[Square]]
goToNextCycle matrix = putRandomTetromino . clearMatrix . groundBlocks matrix


------------ GAME LOGIC ------------


-- TO TEST
--retorna true se tem algum bloco acima do limite da matrix, false caso contrário
isGameOver :: [[Square]] -> Bool
isGameOver matrix = not (all (\k -> getColor k == Black) (concat matrixTop))
    where matrixTop = drop 20 matrix

-- TO TEST
-- desativa todos os blocos
groundBlocks :: [[Square]] -> [[Square]]
groundBlocks [] = []
groundBlocks (x:xs) = map (\Square color active ->Square color (if active == (Active Enable) then (Active Disable) else active)) x ++ groundBlocks xs

-- P
getRandomTetromino :: Tetromino
getRandomTetromino = case randomRIO (0, 6) of
    0 -> [(0,0),(1,0),(2,0),(3,0)] Cyan -- I
    1 -> [(0,0),(1,0),(0,1),(0,2)] Orange -- L
    2 -> [(0,0),(1,0),(0,1),(1,1)] Yellow -- O
    3 -> [(0,0),(1,0),(1,1),(1,2)] Green -- S
    4 -> [(0,0),(1,0),(2,0),(1,1)] Pink -- T
    5 -> [(0,0),(1,0),(1,1),(2,1)] Blue -- J
    6 -> [(0,0),(0,1),(1,1),(1,2)] Red -- Z


-- P
-- bota um tetromino aleatório na matrix.

-- Aqui embaixo acho q vai haver um problema com a estrutura do codigo no geral, eu n acho
-- q a parte q é responsavel por chamar esse comando dnv deve estar aqui dentro, ja que ele (presumo)
-- n é responsavel por checar se a peça ja foi encaixada. A discutir
-- PS eu provavelmente me atrapalhei com os numeros mas isso pode ser concertado rapidamente.
-- So mandar msg.


-- TO TEST
putRandomTetromino :: [[Square]] -> [[Square]]
where newTetronimo = getRandomTetromino
putRandomTetromino x = addBlocks matrix (Square (getTetrominoColor newTetronimo)) (getTetrominoBlocks newTetronimo)


------------ PIECE ROTATION LOGIC ------------

-- TO TEST
canBePutWithSideMove :: [[Square]] -> [(Int, Int)] -> Int
canBePutWithSideMove matrix positions
    | canBePut matrix (map (\k -> ((fst k) - 1, snd k)) positions) = 1
    | canBePut matrix (map (\k -> ((fst k) + 1, snd k)) positions) = 2
    | otherwise = 0

-- TO TEST
-- pega uma matriz e um sentido. retorna essa matriz com os blocos ativos rotacionados pra direita
rotate :: [[Square]] -> [[Square]]
rotate matrix =
  let activeIndexes = findActiveIndexes matrix
  let baseDist = baseDistance matrix
  let zeroedIndexes = map (\x -> subtractTuples x baseDist) activeIndexes
  let rotatedZeroed = rotatePoints zeroedIndexes
  let returnedToPos = addTuples rotatedZeroed baseDist
  let enclosed = encloseCoords returnedToPos
rotate matrix = raiseUntilAllowed matrix enclosed

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

encloseCoords :: [(Int, Int)] -> [(Int, Int)]
encloseCoords coords = newCoords
  where
    minX = min (map (\k -> fst k) coords)
    maxX = max (map (\k -> fst k) coords)
    newCoords
    | minX < 0 = map (\k -> ((fst k) - minX, snd k) coords)
    | maxX >= 10 = map (\k -> ((fst k) - (maxX - 9), snd k) coords)
    | otherwise = coords
  

