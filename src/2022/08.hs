import Lib

type Tree = Int     -- represents height

-- each tree list is a direction (U, D, L, R)
surrounding :: Pos -> Grid Tree -> [[Tree]]
surrounding (Pos x y) ts@(Grid _ h w) =
    [reverse u, d, reverse l, r]
  where
    (u, d) = pierceAt y [ts ! Pos x  y' | y' <- [0 .. h - 1]]
    (l, r) = pierceAt x [ts ! Pos x' y  | x' <- [0 .. w - 1]]

innerIxs :: Grid Tree -> [Pos]
innerIxs (Grid _ h w) =
    [Pos y x | y <- [1 .. h - 2], x <- [1 .. w - 2]]

parseTrees = fromList . mapLines (map digitToInt)

countVisible :: Grid Tree -> Int
countVisible ts@(Grid _ h w) = innerIxs ts
    & filter isVisible
    & length
    & (+ numEdge)
  where
    isVisible p = let t = ts ! p in
        not . all (any (>= t)) $ surrounding p ts
    numEdge = h * 2 + (w - 2) * 2

-- we assume the highest score isn't at the edges
maxScenicScore :: Grid Tree -> Int
maxScenicScore ts =
    maximum $ map scenicScore (innerIxs ts)
  where
    scenicScore p = let t = ts ! p in surrounding p ts
        & map (length . takeWhileInc (< t))
        & product

    takeWhileInc _ [] = []
    takeWhileInc f (x : xs)
        | f x = x : takeWhileInc f xs
        | otherwise = [x]

main = solution 8 $ \input ->
    let trees = parseTrees input
     in ( countVisible   trees
        , maxScenicScore trees )
