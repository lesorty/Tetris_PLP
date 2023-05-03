-- Define a function to find the indices of elements that are equal to s
findIndices :: Int -> [[Int]] -> [(Int, Int)]
findIndices s matrix = do
-- iterate by the rows and columns to get the i and j of the elements
  (i, row) <- zip [0..] matrix
  (j, elem) <- zip [0..] row
  if elem == s then return (i, j) else []

canFallDown :: [[Int]] -> (Int,Int) -> Bool
canFallDown matrix coordinate = if bellow == 2 then False else True
    where bellow = matrix !! ((fst coordinate) + 1) !! (snd coordinate) 

canFallDownBlock :: [[Int]] -> [(Int,Int)] -> Bool
canFallDownBlock matrix [x] =  canFallDown matrix (fst x, snd x)
canFallDownBlock matrix (x:xs) =
    if canFallDown matrix (fst x, snd x) then  canFallDownBlock matrix xs
    else False

fallBlock :: [[Int]] -> [(Int,Int)] -> [(Int,Int)]
fallBlock matrix [x] = [((fst x + 1), snd x)]
fallBlock matrix (x:xs) = ((fst x + 1), snd x) : fallBlock matrix xs

-- Example 2D array
matrix :: [[Int]]
matrix = [[1,1,1,0],[0,1,0,0],[0,0,0,0],[2,2,2,2]]

indices :: [(Int, Int)]
indices = reverse (findIndices 1 matrix)






