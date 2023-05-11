module Main where
import Screen
import MatrixController
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- define os valores do jogo
data GameState = GameState {
    matrix :: [[Square]],
    timeSinceLastDrop :: Float,
    score :: Int,
    droppedPieces :: Int,
    pieceSwap :: Tetromino,
    gameRunning :: Bool
    }

-- define as condições iniciais
newGameState :: GameState
newGameState = GameState { matrix = updatePrediction (putRandomTetromino emptyMatrix 0), timeSinceLastDrop = 0, score = 0, droppedPieces = 0, pieceSwap = getRandomTetromino 0, gameRunning = True }

-- aplica um movimento numa matriz
applyMove :: GameState -> Move -> GameState
applyMove gameState move = gameState { matrix = newGrid, pieceSwap = newPieceSwap }
  where newGrid 
          | move == MoveLeft = if canMoveTetromino grid MoveLeft then moveTetromino grid MoveLeft else grid
          | move == MoveRight = if canMoveTetromino grid MoveRight then moveTetromino grid MoveRight else grid
          | move == MoveDown = if canMoveTetromino grid MoveDown then moveTetromino grid MoveDown else grid
          | move == MoveRotate = rotateTetromino grid
          | move == SuperDown = fullFall grid
          | move == MoveSwap = swapTetromino grid (pieceSwap gameState)
          | otherwise = grid
        grid = matrix gameState
        newPieceSwap = if move == MoveSwap then (Tetromino (bringIndexesToZeroZero (findActiveIndexes (matrix gameState))) (getActiveColor (matrix gameState))) else (pieceSwap gameState)

-- pega o estado que o jogo ficará após o input
nextBoardState :: Event -> GameState -> GameState
nextBoardState (EventKey (Char 'r') Down _ _) gameState = if gameRunning gameState == False then newGameState else gameState
nextBoardState event gameState
  | gameRunning gameState == False = gameState
  | otherwise = gameState { matrix = updatePrediction newMatrix, score = newScore, droppedPieces = newDroppedPieces, pieceSwap = newPieceSwap }
    where
      move = inputToMove event
      oldMatrix = matrix gameState
      cycleEnd = (move == MoveDown && not (canMoveTetromino oldMatrix MoveDown)) || (move == SuperDown)
      postMoveMatrix = matrix (applyMove gameState move)
      newPieceSwap = pieceSwap (applyMove gameState move)
      newScore = if cycleEnd then (score gameState) + pointsForClear (clearableCount postMoveMatrix) else (score gameState)
      newMatrix = if cycleEnd || getActiveColor (matrix gameState) == Black then (goToNextCycle postMoveMatrix seed) else postMoveMatrix
      newDroppedPieces = if cycleEnd then (droppedPieces gameState) + 1 else (droppedPieces gameState)
      seed = ((veryRandom (concat (matrix gameState)) 0) * (round (100.0 * (timeSinceLastDrop gameState)))) `mod` 120189

-- loop do jogo. descida autómatica das peças
progressTime :: Float -> GameState -> GameState 
progressTime deltaTime gameState
    | gameRunning gameState == False = gameState
    | isGameOver (matrix gameState) = gameState {gameRunning = False}
    | (timeSinceLastDrop gameState + deltaTime) > (droppedPiecesToDelay (droppedPieces gameState)) = nextBoardState (EventKey (Char 's') Down a b) gameState { timeSinceLastDrop = 0 }
    | otherwise = gameState { timeSinceLastDrop = (timeSinceLastDrop gameState) + deltaTime }
  where
    a = (Modifiers Down Up Down)
    b = (0,0)

-- define as pontuações de acordo com a quantidade de linhas limpas de uma só vez
pointsForClear :: Int -> Int
pointsForClear 1 = 100
pointsForClear 2 = 250
pointsForClear 3 = 500
pointsForClear 4 = 1000
pointsForClear _ = 0

-- define um delay para que as peças caiam sozinhas baseado na quantidade de peças colocadas em jogo.
droppedPiecesToDelay :: Int -> Float
droppedPiecesToDelay dropped 
  | dropped > 35 = 0.3
  | otherwise = 1.0 - (fromIntegral dropped) / 50.0

-- define as teclas para jogar
inputToMove :: Event -> Move
inputToMove (EventKey (Char 'a') Down _ _) = MoveLeft
inputToMove (EventKey (Char 'd') Down _ _) = MoveRight
inputToMove (EventKey (Char 'w') Down _ _) = MoveRotate
inputToMove (EventKey (Char 's') Down _ _) = MoveDown
inputToMove (EventKey (SpecialKey KeySpace) Down _ _) = SuperDown
inputToMove (EventKey (Char 'c') Down _ _) = MoveSwap
inputToMove _ = MoveNone

-- atualiza a janela com o estado atual do jogo
showGameState :: GameState -> Picture
showGameState gameState
  | gameRunning gameState == False = showGameOver (score gameState)
  | otherwise = showGrid (matrix gameState) (score gameState)

-- main onde ocorre a inicialização do Tetris
main :: IO ()
main = play window backgroundColor 120 newGameState showGameState nextBoardState progressTime
  where backgroundColor = makeColorI 20 20 20 255