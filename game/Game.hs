import Screen
import MatrixController


-- TO TEST
highScoreFile :: FilePath
highScoreFile = "highscore.txt"


-- TO TEST
actionLoop :: [[Square]] -> Move -> [[Square]]
actionLoop matrix input 
    | isGameOver matrix = showGameOver
    | input == (Move Left) = if canMoveTetromino matrix (Move Left) then moveTetromino matrix (Move Left) else matrix
    | input == (Move Right) = if canMoveTetromino matrix (Move Right) then moveTetromino matrix (Move Right) else matrix
    | input == (Move Down) = if canMoveTetromino matrix (Move Down) then moveTetromino matrix (Move Down) else goToNextCycle matrix
    | input == (Move Rotate) = rotate matrix
    | input == (Move SuperDown) = fullFall matrix


-- TO TEST
goToNextCycle :: [[Square]] -> [[Square]]
goToNextCycle matrix = putRandomTetromino . clearMatrix . groundBlocks matrix


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