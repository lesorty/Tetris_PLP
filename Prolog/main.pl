Main

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

-- aplica um movimento numa matriz
applyMove :: GameState -> Move -> GameState

-- pega o estado que o jogo ficará após o input
nextBoardState :: Event -> GameState -> GameState

-- loop do jogo. descida autómatica das peças
progressTime :: Float -> GameState -> GameState 

-- define as pontuações de acordo com a quantidade de linhas limpas de uma só vez
pointsForClear :: Int -> Int

-- define um delay para que as peças caiam sozinhas baseado na quantidade de peças colocadas em jogo.
droppedPiecesToDelay :: Int -> Float

-- define as teclas para jogar
inputToMove :: Event -> Move

-- atualiza a janela com o estado atual do jogo
showGameState :: GameState -> Picture

-- main onde ocorre a inicialização do Tetris
main :: IO ()
