module Screen where

import Graphics.Gloss
import MatrixController

-- showGrid, desenha o grid completo na tela, com divisórias
showGrid :: Int -> [String] -> Picture
showGrid score g =
  let cells = concat [[drawCell (x, y) c | (c, x) <- zip row [0 ..]] ++ [line [(fst (cellToScreen (0, y)) - cellWidth / 2, snd (cellToScreen (0, y)) - cellHeight / 2), (fst (cellToScreen ((length row) - 1, y)) + cellWidth / 2, snd (cellToScreen (0, y)) - cellHeight / 2)]] | (row, y) <- zip g [0 ..]]
      scoreBox = translate (-400) 300 $ scale 0.3 0.3 $ color black $ text $ "Score: " ++ show score
   in pictures [scoreBox, pictures cells]

-- Define a largura e a altura de cada célula do grid
cellWidth :: Float
cellWidth = 30.0

cellHeight :: Float
cellHeight = 30.0

-- Define as cores de cada caractere
colorForChar :: Char -> Color
colorForChar c = case c of
  'a' -> makeColorI 41 178 178 255 -- ciano
  'b' -> makeColorI 0 94 94 255 -- azul escuro
  'c' -> makeColorI 255 102 0 255 -- laranja
  'd' -> makeColorI 255 255 0 255 -- amarelo
  'e' -> makeColorI 0 179 89 255 -- verde
  'f' -> makeColorI 178 41 163 255 -- roxo
  'g' -> makeColorI 255 51 51 255 -- vermelho
  _ -> makeColorI 217 217 217 255 -- cor do fundo

-- Define o array de strings predefinido
grid :: [String]
grid = replicate 20 "abcdefg.ab"

-- Converte uma coordenada de célula (x, y) em uma posição da tela (x, y)
cellToScreen :: (Int, Int) -> (Float, Float)
cellToScreen (x, y) =
  let x' = fromIntegral x * cellWidth - (cellWidth * fromIntegral (length (head grid))) / 2
      y' = fromIntegral y * cellHeight - (cellHeight * fromIntegral (length grid)) / 2
   in (x', y')

-- Desenha uma célula na tela, com divisórias
drawCell :: (Int, Int) -> Char -> Picture
drawCell (x, y) c =
  let col = colorForChar c
      pos = cellToScreen (x, y)
      cell = rectangleSolid cellWidth cellHeight
      -- Adiciona as divisórias da célula
      leftLine = line [(fst pos - cellWidth / 2, snd pos + cellHeight / 2), (fst pos - cellWidth / 2, snd pos - cellHeight / 2)]
      topLine = line [(fst pos - cellWidth / 2, snd pos + cellHeight / 2), (fst pos + cellWidth / 2, snd pos + cellHeight / 2)]
      rightLine = line [(fst pos + cellWidth / 2, snd pos + cellHeight / 2), (fst pos + cellWidth / 2, snd pos - cellHeight / 2)]
      bottomLine = line [(fst pos - cellWidth / 2, snd pos - cellHeight / 2), (fst pos + cellWidth / 2, snd pos - cellHeight / 2)]
   in pictures [leftLine, topLine, rightLine, bottomLine, color col $ uncurry translate pos cell]

-- showGameOver :: ?????