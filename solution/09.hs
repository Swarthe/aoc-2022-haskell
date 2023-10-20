import Lib

import Data.Set (Set)
import qualified Data.Set as Set

data Rope = Rope { h, t :: Pos }

data Pos = Pos { x, y :: Int } deriving (Eq, Ord)

data Motion = U | D | L | R deriving (Read, Eq)

move :: Motion -> Rope -> Rope
move motion (Rope h t) = Rope
    { h = case motion of
        U -> h { y = y h + 1 }
        D -> h { y = y h - 1 }
        L -> h { x = x h - 1 }
        R -> h { x = x h + 1 }
    , t = condShift $ case tIx of
        0 -> [U, R];  1 -> [U];  2 -> [U, L]
        3 -> [R];     4 -> [];   5 -> [L]
        6 -> [D, R];  7 -> [D];  8 -> [D, L] }
  where condShift motions
            -- if the tail moves, it takes the former place of the head
            | motion `elem` motions = h
            | otherwise = t                 -- 6 7 8
        tIx = let Pos x y = t `diff` h in   -- 3 4 5
            (x + 1) + (y + 1) * 3           -- 0 1 2
        diff (Pos x y) (Pos x' y') =
            Pos (x - x') (y - y')

parseMotions :: String -> [Motion]
parseMotions = concat . mapLines parseMotion
  where parseMotion (dir : _ : count) =
            replicate (read count) (read [dir])

-- positions visited by the tail
visitedPositions :: [Motion] -> Set Pos
visitedPositions = go $ Rope (Pos 0 0) (Pos 0 0)
  where go rope (m : ms) = let moved = move m rope in
            Set.singleton (t moved) `Set.union` go moved ms
        go _ [] = Set.empty

main = do
    motions <- parseMotions <$> readInput 9

    print $ Set.size (visitedPositions motions)
