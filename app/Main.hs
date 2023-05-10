module Main where
import Screen
import MatrixController
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data GameState = GameState {
    matrix :: [[Square]],
    timeSinceLastDrop :: Float,
    score :: Int,
    droppedPieces :: Int,
    gameRunning :: Bool
    }

newGameState :: GameState
--newGameState = GameState { matrix = putRandomTetromino emptyMatrix, timeSinceLastDrop = 0, score = 0 }
newGameState = GameState { matrix = updatePrediction (putRandomTetromino emptyMatrix 0), timeSinceLastDrop = 0, score = 0, droppedPieces = 0, gameRunning = True }


-- TO TEST
highScoreFile :: FilePath
highScoreFile = "highscore.txt"

applyMove :: [[Square]] -> Move -> [[Square]]
applyMove grid move
    | move == MoveLeft = if canMoveTetromino grid MoveLeft then moveTetromino grid MoveLeft else grid
    | move == MoveRight = if canMoveTetromino grid MoveRight then moveTetromino grid MoveRight else grid
    | move == MoveDown = if canMoveTetromino grid MoveDown then moveTetromino grid MoveDown else grid
    | move == MoveRotate = rotateTetromino grid
    | move == SuperDown = fullFall grid
    | otherwise = grid

-- TO TEST
nextBoardState :: Event -> GameState -> GameState
nextBoardState event gameState
  | gameRunning gameState == False = gameState
  | otherwise = GameState { matrix = updatePrediction newMatrix, timeSinceLastDrop = newTime, score = newScore, droppedPieces = newDroppedPieces, gameRunning = True}
    where
      move = inputToMove event
      oldMatrix = matrix gameState
      cycleEnd = (move == MoveDown && not (canMoveTetromino oldMatrix MoveDown)) || (move == SuperDown)
      postMoveMatrix = applyMove oldMatrix move
      newTime = timeSinceLastDrop gameState
      newScore = if cycleEnd then (score gameState) + pointsForClear (clearableCount postMoveMatrix) else (score gameState)
      newMatrix = if cycleEnd then (goToNextCycle postMoveMatrix seed) else postMoveMatrix
      newDroppedPieces = if cycleEnd then (droppedPieces gameState) + 1 else (droppedPieces gameState)
      seed = ((veryRandom (concat (matrix gameState)) 0) * (round (100.0 * (timeSinceLastDrop gameState)))) `mod` 120189

progressTime :: Float -> GameState -> GameState 
progressTime deltaTime gameState
    | gameRunning gameState == False = gameState
    | isGameOver (matrix gameState) = GameState { matrix = matrix gameState, timeSinceLastDrop = timeSinceLastDrop gameState, score = score gameState, droppedPieces = droppedPieces gameState, gameRunning = False }
    | otherwise = GameState { matrix = updatePrediction newMatrix, timeSinceLastDrop = newTime, score = newScore, droppedPieces = droppedPieces gameState, gameRunning = True}
      where
        forceFall = (timeSinceLastDrop gameState + deltaTime) > (droppedPiecesToDelay (droppedPieces gameState))
        newTime = if forceFall then 0 else (timeSinceLastDrop gameState + deltaTime)
        newMatrix = if forceFall then (matrix (nextBoardState (EventKey (Char 's') Down a b) gameState)) else matrix gameState
        --newMatrix = if forceFall then applyMove (matrix gameState) MoveRight else matrix gameState
        --newMatrix = if forceFall then emptyMatrix else matrix gameState

        newScore = if forceFall then (score (nextBoardState (EventKey (Char 's') Down a b) gameState)) else (score gameState)
        a = (Modifiers Down Up Down)
        b = (0,0)

pointsForClear :: Int -> Int
pointsForClear 1 = 100
pointsForClear 2 = 250
pointsForClear 3 = 500
pointsForClear 4 = 1000
pointsForClear _ = 0

droppedPiecesToDelay :: Int -> Float
droppedPiecesToDelay dropped 
  | dropped > 35 = 0.3
  | otherwise = 1.0 - (fromIntegral dropped) / 50.0

--progressTime deltaTime gameState = gameState

inputToMove :: Event -> Move
inputToMove (EventKey (Char 'a') Down _ _) = MoveLeft
inputToMove (EventKey (Char 'd') Down _ _) = MoveRight
inputToMove (EventKey (Char 'w') Down _ _) = MoveRotate
inputToMove (EventKey (Char 's') Down _ _) = MoveDown
inputToMove (EventKey (SpecialKey KeySpace) Down _ _) = SuperDown
inputToMove _ = MoveNone

-- TO TEST
getHighScore :: Int
getHighScore = 100
--getHighScore = do
-- fileExists <- doesFileExist highScoreFile
--  if fileExists
--    then withFile highScoreFile ReadMode $ \handle -> do
--      contents <- hGetContents handle
--      return (read contents)
--    else return 0

-- TO TEST
updateHighScore :: Int
updateHighScore = 200
--updateHighScore score = do
--  withFile highScoreFile WriteMode $ \handle -> do
--    hPrint handle score

showGameState :: GameState -> Picture
showGameState gameState
  | gameRunning gameState == False = showGameOver (score gameState) getHighScore
  | otherwise = showGrid (matrix gameState) (score gameState)
main :: IO ()
main = play window backgroundColor 120 newGameState showGameState nextBoardState progressTime
  where backgroundColor = makeColorI 40 40 40 255