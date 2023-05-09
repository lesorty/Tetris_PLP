import System.IO
import Control.Concurrent
import Screen
import MatrixController

data GameState = GameState {
    matrix :: [[Square]]
    ticksSinceLastDrop :: Int
    score :: Int
}

newGameState :: GameState
newGameState = GameState { matrix = putRandomTetromino emptyMatrix, ticksSinceLastDrop = 0, score = 0 }

-- TO TEST
highScoreFile :: FilePath
highScoreFile = "highscore.txt"

applyMove :: [[Square]] -> Move -> [[Square]]
applyMove matrix move
    | move == MoveLeft = if canMoveTetromino matrix MoveLeft then moveTetromino matrix MoveLeft else matrix
    | move == MoveRight = if canMoveTetromino matrix MoveRight then moveTetromino matrix MoveRight else matrix
    | move == MoveDown = if canMoveTetromino matrix MoveDown then moveTetromino matrix MoveDown else matrix
    | move == MoveRotate = rotate matrix
    | move == SuperDown = fullFall matrix
    | otherwise = matrix

-- TO TEST
nextBoardState :: Event -> GameState -> GameState
nextBoardState event gameState = GameState { matrix = newMatrix, ticksSinceLastDrop = newTicks, score = newScore }
where
    move = inputToMove event
    oldMatrix = matrix gameState
    cycleEnd = (move == MoveDown && not (canMoveTetromino oldMatrix MoveDown)) || (move == SuperDown)
    postMoveMatrix = applyMove oldMatrix move
    newTicks = ticksSinceLastDrop gameState
    newScore = if cycleEnd then (score gameState) + clearableLineCount postMoveMatrix else (score gameState)
    newMatrix = if cycleEnd then goToNextCycle postMoveMatrix else postMoveMatrix

progressTicks :: Float -> GameState -> GameState 
progressTicks deltaTime gameState = GameState { matrix = newMatrix, ticksSinceLastDrop = newTicks, score = newScore }
where
    forceFall = (ticksSinceLastDrop gameState + deltaTime) > 1
    newTicks = if forceFall then 0 else (ticksSinceLastDrop gameState + deltaTime)
    newMatrix = if forceFall then (matrix (nextBoardState (EventKey (Char 's') Down _ _) gameState)) else matrix gameState
    newScore = if forceFall then (score (nextBoardState (EventKey (Char 's') Down _ _) gameState)) else (score gameState)

inputToMove :: Event -> Move
inputToMove (EventKey (Char 'a') Down _ _) = MoveLeft
inputToMove (EventKey (Char 'd') Down _ _) = MoveRight
inputToMove (EventKey (Char 'w') Down _ _) = MoveRotate
inputToMove (EventKey (Char 's') Down _ _) = MoveDown
inputToMove (EventKey (SpecialKey KeySpace) Down _ _) = SuperBaixo
inputToMove _ = MoveNone

-- TO TEST
getHighScore :: Int
getHighScore = do
  fileExists <- doesFileExist highScoreFile
  if fileExists
    then withFile highScoreFile ReadMode $ \handle -> do
      contents <- hGetContents handle
      return (read contents)
    else return 0

-- TO TEST
updateHighScore :: Int
updateHighScore score = do
  withFile highScoreFile WriteMode $ \handle -> do
    hPrint handle score

showGameState :: GameState -> Picture
showGameState gameState = showGrid (take 20 (matrix gameState)) (score gameState)

main :: IO ()
main = play window black 30 newGameState showGameState nextBoardState [progressTicks]