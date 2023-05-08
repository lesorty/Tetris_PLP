import Screen
import MatrixController
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO

-- Eu nao tenho certeza se essa implementação vai ser a melhor sae é q tem mais de uma forma de fazer,
-- mas de toda forma ta aqui
-- TO TEST
type Highscores = Map String Int
filePath :: FilePath
filePath :: "highscores.txt"

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
getHighscores :: Highscores
getHighscores = do
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      contents <- readFile filePath
      return (Map.fromList (read contents))
    else return Map.empty


-- Simples, se o nome ja esta registrado nos highscores e o score a ser adicionado é menor do que a que
-- já estava lá, o novo score não é registrado, caso contrário, ele é registrado novamente 
-- Vai ser necessário pedir o nome do jogador em algum lugar. Possivelmente  uma tela com "Save highscore?".
-- A ser discutido.
-- TO TEST
updateHighscores :: Highscores -> String -> Int -> Highscores
updateHighscores highscores name newscore = 
    if Map.member name highscores == True && newscore > Map.lookup name highscores
        then Map.insertWith max name score highscores
        else return


playMusic