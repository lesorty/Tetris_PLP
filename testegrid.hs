import Graphics.Gloss

-- tamanho de cada quadrado
squareSize :: Float
squareSize = 100

-- distância entre os quadrados
squareGap :: Float
squareGap = 110

-- coordenadas dos quadrados
squareCoords :: [(Float, Float)]
squareCoords = [(x, y) | x <- [-squareGap, 0, squareGap], y <- [-squareGap, 0, squareGap]]

-- desenha um quadrado colorido
coloredSquare :: Color -> Picture
coloredSquare c = color c $ rectangleSolid squareSize squareSize

-- cores dos quadrados
colors :: [Color]
colors = [blue, red, green, orange, yellow, white, magenta, cyan, violet]

-- desenha todos os quadrados com as cores especificadas
coloredSquares :: Picture
coloredSquares = pictures [translate x y (coloredSquare c) | ((x, y), c) <- zip squareCoords colors]

-- main que desenha os quadrados num fundo preto
main :: IO ()
main = display (InWindow "9 quadrados" (600, 600) (10, 10)) black coloredSquares
