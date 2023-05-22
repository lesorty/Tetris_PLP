MAtrixControler


data Move = MoveNone | MoveLeft | MoveRight | MoveRotate | MoveDown | SuperDown | MoveSwap deriving Eq
data BlockColor = Black | Blue | Cyan | Orange | Yellow | Green | Violet | Red deriving (Eq, Show)
data Active = Prediction | Enable | Disable | None deriving Eq
data Square = Square BlockColor Active deriving Eq
data Tetromino = NullTetromino | Tetromino [(Int,Int)] BlockColor deriving Eq

-- retorna a cor de um quadrado
getColor :: Square -> BlockColor

-- função para gerar números aleatorios (logica usada sempre que aparece uma nova peça)
veryRandom :: [Square] -> Int -> Int


-- retorna o estado de um quadrado
getActive :: Square -> Active

-- retorna a cor dos quadrados que estão caindo
getActiveColor :: [[Square]] -> BlockColor

-- retorna uma linha vazia
emptyLine :: [Square]

-- retorna uma matriz vazia
emptyMatrix :: [[Square]]

-- retorna as coordenadas de um tetromino
getTetrominoBlocks :: Tetromino -> [(Int, Int)]

-- retorna a cor de um tetromino
getTetrominoColor :: Tetromino -> BlockColor

-- retorna as coordenadas onde existem blocos caindo
findActiveIndexes :: [[Square]] -> [(Int, Int)]


------------ PIECE PERMISSION LOGIC ------------


--retorna se um conjunto de blocos pode ser colocado na matrix.
canBePut :: [[Square]] -> [(Int, Int)] -> Bool

-- retorna se é possível mover o tetromino para certa posição
canMoveTetromino :: [[Square]] -> Move -> Bool



------------ PIECE MOVEMENT LOGIC ------------


-- atualiza um elemento da matriz (magia!).
updateMatrixElement :: [[Square]] -> (Int, Int) -> Square -> [[Square]]

-- retorna a matriz sem blocos ativos
removeActiveBlocks :: [[Square]] -> [[Square]]

-- retorna a matriz sem blocos de previsão
removePrediction :: [[Square]] -> [[Square]]

-- adiciona o quadrado do parametro nas coordenadas também passadas como parametro
addBlocks :: [[Square]] -> Square -> [(Int, Int)] -> [[Square]]

-- retorna as coordenadas finais de um movimento
getEndPos :: [[Square]] -> Move -> [(Int, Int)]

-- remove todos os blocos ativos. bota blocos ativos nas posiçoes indicadas
changeActiveBlocksPos :: [[Square]] -> [(Int, Int)] -> [[Square]]

-- movimenta o Tetromino
moveTetromino :: [[Square]] -> Move -> [[Square]]

-- pega as coordenadas dos blocos de previsão
getPredictionCoords :: [[Square]] -> [(Int, Int)]

-- atualiza a previsão de acordo com a nova matriz
updatePrediction :: [[Square]] -> [[Square]]

-- derruba a peça instantaneamente
fullFall :: [[Square]] -> [[Square]]


------------ CLEAR MATRIX LOGIC ------------


-- pega uma matriz e um índice. retorna se essa linha é clearável ou não
canClearLine :: [Square] -> Bool

-- pega uma matriz e uma lista de índices. retorna uma matriz com todos esses índices clearados
clearMatrix :: [[Square]] -> [[Square]]

-- retorna a quantidade de linhas que podem ser limpas de uma vez
clearableCount :: [[Square]] -> Int

-- reinicia o ciclo chamando uma nova peça e limpando as linhas completas
goToNextCycle :: [[Square]] -> Int -> [[Square]]


------------ GAME LOGIC ------------

--retorna true se tem algum bloco acima do limite da matriz, false caso contrário
isGameOver :: [[Square]] -> Bool

-- para cada quadrado da matriz, desativa caso seja ativo
groundBlocks :: [[Square]] -> [[Square]]

-- gera uma peça aleatória
getRandomTetromino :: Int -> Tetromino

-- coloca em jogo uma peça aleatória
putRandomTetromino :: [[Square]] -> Int -> [[Square]]

-- troca a peça atual por uma reserva
swapTetromino :: [[Square]] -> Tetromino -> [[Square]]


------------ PIECE ROTATION LOGIC ------------


-- verifica se a peça pode ser colocada apenas com um movimento lateral
canBePutWithSideMove :: [[Square]] -> [(Int, Int)] -> Int

-- pega uma matriz. retorna essa matriz com os blocos ativos rotacionados pra direita
rotateTetromino :: [[Square]] -> [[Square]]

-- deixa o ponto superior esquerdo de um conjunto de coordenadas zerado. todas as outras relações se mantém
bringIndexesToZeroZero :: [(Int, Int)] -> [(Int, Int)]s

-- pega um conjunto de pontos na matriz. retorna, o ponto no top left
baseDistance :: [(Int, Int)] -> (Int, Int)

-- subtrai tuplas
subtractTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)

-- soma tuplas
addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)

-- rotaciona um conjunto de pontos no sentido horário
rotatePoints :: [(Int, Int)] -> [(Int, Int)]

--pega um conjunto de coordenadas. sobe elas até que possa botar elas na matriz, então retorna as novas coordenadas
raiseUntilAllowed :: [[Square]] -> [(Int, Int)] -> [(Int, Int)]

-- coloca coordenadas dentro dos limites da matriz
encloseCoords :: [(Int, Int)] -> [(Int, Int)]
