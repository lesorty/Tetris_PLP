gridToString :: [[String]] -> String
gridToString [] = ""
gridToString (x:xs) = concat x ++ "\n" ++ gridToString xs

emptyGrid :: [[String]]
emptyGrid = replicate 8 (replicate 8 ".")

fallDown :: [[String]] -> [[String]]
fallDown [] = []
fallDown [x] = [x]
fallDown [x,y] = fallDown2 x y
fallDown (x:xs) =  fallDown2 x (head (fallDown xs)) ++ (tail (fallDown xs ))

fallDown2 :: [String] -> [String] -> [[String]]
fallDown2 a [] = [a ++ []]
fallDown2 [] b = [[] ++ b]
fallDown2 (a:at) (b:bt) 
    | a /= "." && b == "." = [b : rest1, a : rest2]
    | otherwise = [a : rest1, b : rest2]
    where rest = fallDown2 at bt
          rest1 = if null rest then [] else head rest
          rest2 = if length rest < 2 then [] else rest !! 1


main :: IO ()
main = do
    putStrLn (gridToString (fallDown [[".", ".", ".", ".", "a", "b", "c", "d"], [".", ".", "e", "f", ".", ".", "g", "h"], [".", "i", ".", "j", ".", "k", ".", "l"]] ))