module Screen where

import MatrixController

-- PLACEHOLDER
showGrid :: [[Square]] -> String
showGrid grid =
  let colorToChar Empty = '.'
      colorToChar Blue = '1'
      colorToChar Cyan = '2'
      colorToChar Orange = '3'
      colorToChar Yellow = '4'
      colorToChar Green = '5'
      colorToChar Violet = '6'
      colorToChar Red = '7'
      rowToString row = map (\(Square color _) -> colorToChar color) row
  in concatMap (\row -> rowToString row ++ "\n") grid

-- showGameOver :: ?????