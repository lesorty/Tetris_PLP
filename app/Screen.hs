module Screen where

import Graphics.Gloss
import MatrixController

-- showGrid, desenha o grid completo na tela, com divisórias
showGrid :: [[Square]] -> Int -> Picture
showGrid matrix score =
  let g = take 20 matrix
      cells = concat [[drawCell (x, y) c | (c, x) <- zip row [0 ..]] ++ [line [(fst (cellToScreen (0, y)) - cellWidth / 2, snd (cellToScreen (0, y)) - cellHeight / 2), (fst (cellToScreen ((length row) - 1, y)) + cellWidth / 2, snd (cellToScreen (0, y)) - cellHeight / 2)]] | (row, y) <- zip g [0 ..]]
      scoreBox = translate (-400) 300 $ scale 0.3 0.3 $ color white $ text $ "Score: " ++ show score
   in pictures [scoreBox, pictures cells]

-- Define a largura e a altura de cada célula do grid
cellWidth :: Float
cellWidth = 30.0

cellHeight :: Float
cellHeight = 30.0


window :: Display
window = InWindow "My Game" (550, 800) (10, 10)

-- Define as cores de cada caractere
colorForSquare :: Square -> Color
colorForSquare square
  | blockColor == Cyan = makeColorI 0 245 230 a -- ciano
  | blockColor == Blue = makeColorI 0 20 245 a -- azul escuro
  | blockColor == Orange = makeColorI 255 102 0 a -- laranja
  | blockColor == Yellow = makeColorI 230 230 0 a -- amarelo
  | blockColor == Green = makeColorI 0 255 0 a -- verde
  | blockColor == Violet = makeColorI 224 12 245 a -- roxo
  | blockColor == Red = makeColorI 255 0 0 a -- vermelho
  | otherwise = makeColorI 0 0 0 a -- cor do fundo
  where 
    blockColor = getColor square
    a = if getActive square == Prediction then 127 else 255

-- Define o array de strings predefinido
--grid :: [String]
--grid = replicate 20 "abcdefg.ab"

-- Converte uma coordenada de célula (x, y) em uma posição da tela (x, y)
cellToScreen :: (Int, Int) -> (Float, Float)
cellToScreen (x, y) =
  let x' = fromIntegral x * cellWidth - (cellWidth * 10) / 2
      y' = fromIntegral y * cellHeight - (cellHeight * 20) / 2
   in (x', y')

-- Desenha uma célula na tela, com divisórias
drawCell :: (Int, Int) -> Square -> Picture
drawCell (x, y) sq =
  let col = colorForSquare sq
      pos = cellToScreen (x, y)
      cell = rectangleSolid cellWidth cellHeight
      -- Adiciona as divisórias da célula
      leftLine = line [(fst pos - cellWidth / 2, snd pos + cellHeight / 2), (fst pos - cellWidth / 2, snd pos - cellHeight / 2)]
      topLine = line [(fst pos - cellWidth / 2, snd pos + cellHeight / 2), (fst pos + cellWidth / 2, snd pos + cellHeight / 2)]
      rightLine = line [(fst pos + cellWidth / 2, snd pos + cellHeight / 2), (fst pos + cellWidth / 2, snd pos - cellHeight / 2)]
      bottomLine = line [(fst pos - cellWidth / 2, snd pos - cellHeight / 2), (fst pos + cellWidth / 2, snd pos - cellHeight / 2)]
   in pictures [leftLine, topLine, rightLine, bottomLine, color col $ uncurry translate pos cell]

-- exibe a tela de game over com a pontuação atual e highscore
showPreviousHighscore :: Int -> Picture
showPreviousHighscore x = 
  pictures [
    translate (-200) (-150) $ color white $ scale 0.2 0.2 $ text ("O seu high score eh de: "),
    translate (150) (-150) $ color yellow $ scale 0.2 0.2 $ text (show x)
  ]

showGameOver :: Int -> Int -> Picture
showGameOver pontosAtual highScore = pictures [
      translate (-210) 0 $ color red $ scale 0.5 0.5 $ text "Game Over! :(",
      translate (-200) (-50) $ color white $ scale 0.25 0.25 $ text "pressione ESC para sair",
      translate (-200) (-100) $ color white $ scale 0.2 0.2 $ text ("A sua pontuacao foi de: "),
      translate (150) (-100) $ color yellow $ scale 0.2 0.2 $ text (show pontosAtual),
      if pontosAtual > highScore then
        translate (-175) (-150) $ color green $ scale 0.2 0.2 $ text "Isso eh um novo highscore!!"
      else
        showPreviousHighscore highScore
  ]