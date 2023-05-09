module Main where
import System.IO
import Control.Concurrent
import Screen
import MatrixController
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data GameState = GameState {
    matrix :: [[Square]],
    timeSinceLastDrop :: Float,
    score :: Int
    }

newGameState :: GameState
newGameState = GameState { matrix = putRandomTetromino emptyMatrix, timeSinceLastDrop = 0, score = 0 }

-- TO TEST
highScoreFile :: FilePath
highScoreFile = "highscore.txt"

applyMove :: [[Square]] -> Move -> [[Square]]
applyMove matrix move
    | move == MoveLeft = if canMoveTetromino matrix MoveLeft then moveTetromino matrix MoveLeft else matrix
    | move == MoveRight = if canMoveTetromino matrix MoveRight then moveTetromino matrix MoveRight else matrix
    | move == MoveDown = if canMoveTetromino matrix MoveDown then moveTetromino matrix MoveDown else matrix
    | move == MoveRotate = rotateTetromino matrix
    | move == SuperDown = fullFall matrix
    | otherwise = matrix

-- TO TEST
nextBoardState :: Event -> GameState -> GameState
nextBoardState event gameState = GameState { matrix = newMatrix, timeSinceLastDrop = newTime, score = newScore }
  where
    move = inputToMove event
    oldMatrix = matrix gameState
    cycleEnd = (move == MoveDown && not (canMoveTetromino oldMatrix MoveDown)) || (move == SuperDown)
    postMoveMatrix = applyMove oldMatrix move
    newTime = timeSinceLastDrop gameState
    newScore = if cycleEnd then (score gameState) + clearableCount postMoveMatrix else (score gameState)
    newMatrix = if cycleEnd then goToNextCycle postMoveMatrix else postMoveMatrix

progressTime :: Float -> GameState -> GameState 
progressTime deltaTime gameState = GameState { matrix = newMatrix, timeSinceLastDrop = newTime, score = newScore }
  where
    forceFall = (timeSinceLastDrop gameState + deltaTime) > 1
    newTime = if forceFall then 0 else (timeSinceLastDrop gameState + deltaTime)
    newMatrix = if forceFall then (matrix (nextBoardState (EventKey (Char 's') Down a b) gameState)) else matrix gameState
    newScore = if forceFall then (score (nextBoardState (EventKey (Char 's') Down a b) gameState)) else (score gameState)
    a = (Modifiers Down Up Down)
    b = (0,0)
inputToMove :: Event -> Move
inputToMove (EventKey (Char 'a') Down _ _) = MoveLeft
inputToMove (EventKey (Char 'd') Down _ _) = MoveRight
inputToMove (EventKey (Char 'w') Down _ _) = MoveRotate
inputToMove (EventKey (Char 's') Down _ _) = MoveDown
inputToMove (EventKey (SpecialKey KeySpace) Down _ _) = SuperDown
inputToMove _ = MoveNone

-- TO DO
getHighScore :: Int
getHighScore = 100
--getHighScore = do
-- fileExists <- doesFileExist highScoreFile
--  if fileExists
--    then withFile highScoreFile ReadMode $ \handle -> do
--      contents <- hGetContents handle
--      return (read contents)
--    else return 0

-- TO DO
updateHighScore :: Int
updateHighScore = 200
--updateHighScore score = do
--  withFile highScoreFile WriteMode $ \handle -> do
--    hPrint handle score

showGameState :: GameState -> Picture
showGameState gameState
  | isGameOver (matrix gameState) = showGameOver (score gameState) getHighScore
  | otherwise = showGrid (take 20 (matrix gameState)) (score gameState)

main :: IO ()
main = play window black 30 newGameState showGameState nextBoardState progressTime