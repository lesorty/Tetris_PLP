Screen

-- showGrid, desenha o grid completo na tela
showGrid :: [[Square]] -> Int -> Picture

-- Define a largura e a altura de cada célula do grid
cellWidth :: Float

-- a janela do jogo
window :: Display

-- define as cores de cada quadrado
colorForSquare :: Square -> Color

-- converte uma coordenada de célula (x, y) em uma posição da tela (x, y)
cellToScreen :: (Int, Int) -> (Float, Float)

-- desenha uma célula na tela, com divisórias
drawCell :: (Int, Int) -> Square -> Picture
   
-- exibe a tela de game over com a pontuação atual
showGameOver :: Int -> Picture