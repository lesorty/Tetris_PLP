import System.IO
import Control.Concurrent
import Screen
import MatrixController

data GameState = GameState {
    matrix :: [[Square]]
    ticksSinceLastDrop :: Int
    score :: Int
}

-- TO TEST
highScoreFile :: FilePath
highScoreFile = "highscore.txt"


-- TO TEST
nextBoardState :: [[Square]] -> Move -> [[Square]]
nextBoardState matrix input 
    | isGameOver matrix = showGameOver
    | input == (Move Left) = if canMoveTetromino matrix (Move Left) then moveTetromino matrix (Move Left) else matrix
    | input == (Move Right) = if canMoveTetromino matrix (Move Right) then moveTetromino matrix (Move Right) else matrix
    | input == (Move Down) = if canMoveTetromino matrix (Move Down) then moveTetromino matrix (Move Down) else goToNextCycle matrix
    | input == (Move Rotate) = rotate matrix
    | input == (Move SuperDown) = fullFall matrix


-- TO TEST
goToNextCycle :: [[Square]] -> [[Square]]
goToNextCycle matrix = putRandomTetromino . clearMatrix . groundBlocks matrix

main :: IO ()
main = play window black 30 emptyMatrix showGrid (\event matrix -> nextBoardState matrix (inputToMove event)) (\_ state -> state)
    
  
    




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
