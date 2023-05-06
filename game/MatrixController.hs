module MatrixController where
  
data Move = Left | Right | Rotate | Down | SuperDown
data Color = Empty | Blue | Cyan | Orange | Yellow | Green | Violet | Red deriving Eq
data Piece = None | LeftL | RightL | TwoByTwo | Rectangule | LeftS | RigthS | T deriving Eq
data Falling = Enable | Disable deriving Eq
data Square = Square Color Falling deriving Eq

-- L: Essa função está bugada
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

--move todos os blocos ativos numa direção. presume que eles podem ser movidos
moveTetromino :: [[Square]] -> Move -> [[Square]]

-- Pega uma matriz e retorna a lista de linhas que podem ser limpas
clearableLines :: [[Square]] -> [Int]
-- usa canClearLine

-- pega uma matriz e um índice. retorna se essa linha é clearável ou não
canClearLine :: [[Square]] -> Int -> Bool

-- pega uma matriz e uma lista de índices. retorna uma matriz com todos esses índices clearados
clearTheseLines :: [[Square]] -> [Int] -> [[Square]]

-- pega uma matriz e retorna ela com todas as linhas clearáveis clearadas
clearMatrix :: [[Square]] -> ([[Square]], Int)
-- clearMatrix grid = 
    -- x = clearableLines grid
    -- clearedMatrix = clearTheseLines grid x
    -- return clearedMatrix 


getColor :: Square -> Color
getColor (Square color _) = color

--retorna se um conjunto de blocos pode ser colocado na grid.
canBePut :: [[Square]] -> [(Int, Int)] -> Bool

--retorna 1 se pode ser posto com um movimento pra esquerda. 2 com um pra direita. 0 se não pode
canBePutWithSideMove :: [[Square]] -> [(Int, Int)] -> Int


-- remove todos os blocos ativos. bota blocos ativos nas posiçoes indicadas
changeActiveBlocksPos :: [[Square]] -> [(Int, Int)] -> [[Square]]

--retorna true se tem algum bloco acima do limite da grid, false caso contrário
isGameOver :: [[Square]] -> Bool

-- Retorna a grid sem blocos ativos
removeActiveBlocks :: [[Square]] -> [[Square]]

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


------------ PIECE ROTATION LOGIC ------------

-- pega uma matriz e um sentido. retorna essa matriz com os blocos ativos rotacionados pra direita
rotate :: [[Square]] -> [[Square]]
rotate grid =
  let activeIndexes = findIndexes grid
  let baseDist = baseDistance grid
  let zeroedIndexes = map (\x -> subtractTuples x baseDist) activeIndexes
  let rotatedZeroed = rotatePoints zeroedIndexes
  let returnedToPos = rotatePoints
rotate grid = raiseUntilAllowed grid returnedToPos

-- pega um conjunto de pontos na grid. retorna, dentre os pontos mais baixos, o mais à esquerda
baseDistance :: [(Int, Int)] -> (Int, Int)
baseDistance cloud =
  let base = filter (\x -> snd x == min (map snd cloud))
      maisEsquerdaDaBase = filter (\x -> fst x == min (map fst base)) base
  in head maisEsquerdaDaBase

subtractTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
subtractTuples (a, b) (c, d) = (a - c, b - d)

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (a, b) (c, d) = (a + c, b + d)

-- rotaciona um conjunto de pontos no sentido horário
rotatePoints :: [(Int, Int)] -> [(Int, Int)]
rotatePoints cloud = map (\x -> (snd x, -(fst x))) cloud

--pega um conjunto de coordenadas. sobe elas até que possa botar elas na matriz, então bota elas na matriz
raiseUntilAllowed :: [[Square]] -> [(Int, Int)] -> [[Square]]
raiseUntilAllowed grid coords 
  | canBePut grid coords = changeActiveBlocksPos grid coords
  | canBePutWithSideMove grid coords == 1 = changeActiveBlocksPos grid (map (\k -> (((fst k) - 1), (snd k))) coords)
  | canBePutWithSideMove grid coords == 2 = changeActiveBlocksPos grid (map (\k -> (((fst k) + 1), (snd k))) coords)
  | otherwise = raiseUntilAllowed grid (map (\k -> ((fst k), ((snd k) + 1))) coords)
