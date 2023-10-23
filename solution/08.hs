import Lib

type Tree = Int     -- represents height

surrounding :: (Int, Int) -> [[Tree]] -> [[Tree]]
surrounding (y, x) ts = [reverse l, r, reverse u, d]
  where (l, r) = pierceAt x (ts !! y)
        (u, d) = pierceAt y [ts !! y' !! x | y' <- [0 .. length ts - 1]]

innerIxs :: [[Tree]] -> [(Int, Int)]
innerIxs ts = [(y, x) | y <- [1 .. length ts - 2],
                        x <- [1 .. length (head ts) - 2]]

parseTrees :: String -> [[Tree]]
parseTrees = mapLines (map $ read . (:[]))

countVisible :: [[Tree]] -> Int
countVisible ts = innerIxs ts
    & filter isVisible
    & length
    & (+ numEdge)
  where isVisible (y, x) = let t = ts !! y !! x in
             not . all (any (>= t)) $ surrounding (y, x) ts
        numEdge = length ts * 2 + (length (head ts) - 2) * 2

-- we assume the highest score isn't at the edges
maxScenicScore :: [[Tree]] -> Int
maxScenicScore ts = maximum $ map scenicScore (innerIxs ts)
  where scenicScore (y, x) = let t = ts !! y !! x in
            surrounding (y, x) ts
                & map (length . takeWhileInc (< t))
                & product
        takeWhileInc _ [] = []
        takeWhileInc f (x : xs) =
            if f x then x : takeWhileInc f xs
                   else [x]

main = solvePuzzle 8 $ \input ->
    let trees = parseTrees input
     in ( countVisible trees
        , maxScenicScore trees )
