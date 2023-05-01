module DrawGrid where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Estado inicial do sistema
data State = State {position :: (Int, Int)}

-- Função que desenha o sistema
drawGrid :: Picture
drawGrid = pictures $ map (color red . drawLine) gridLines
  where
    drawLine (x1, y1, x2, y2) = line [(x1, y1), (x2, y2)]
    gridLines =
      [(-450, y, 450, y) | y <- [-450, -350 .. 450]]
        ++ [(x, -450, x, 450) | x <- [-450, -350 .. 450]]

-- Função que atualiza o estado do sistema de acordo com o input do usuário
updateState :: Event -> State -> State
updateState (EventKey (Char 'w') _ _ _) (State (x, y)) = State (x, y + 1)
updateState (EventKey (Char 'a') _ _ _) (State (x, y)) = State (x - 1, y)
updateState (EventKey (Char 's') _ _ _) (State (x, y)) = State (x, y - 1)
updateState (EventKey (Char 'd') _ _ _) (State (x, y)) = State (x + 1, y)
updateState _ s = s

-- Função que desenha a situação atualizada em cada segundo
drawScene :: State -> Picture
drawScene (State (x, y)) = pictures [drawGrid, translate (fromIntegral x * 50) (fromIntegral y * 50) target]
  where
    target = color white $ rectangleSolid 50 50