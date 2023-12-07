import Lib

import Data.Set (Set)
import qualified Data.Set as Set

type Rope = [Pos]
type Offset = Pos   -- values within [-1 .. 1]

move :: Offset -> Rope -> Rope
move offset (h : ts) =
    scanl follow (h + offset) ts
  where
    follow h t
        | let Pos x y = h - t in
          all (inRange (-1, 1)) [x, y] = t          -- adjacency
        | otherwise = h + case h - t of
            Pos x y | (abs x, abs y) == (2, 2)      -- corners
                   -> Pos (-signum x) (-signum y)
            Pos x _ | x > 1  -> Pos (-1)  0         -- edges
                    | x < -1 -> Pos   1   0
            Pos _ y | y > 1  -> Pos   0 (-1)
                    | y < -1 -> Pos   0   1

parseOffsets = concat . mapLines parseGroup
  where
    parseGroup (dir : _ : n) =
        replicate (read n) $ case dir of
            'U' -> Pos   0   1
            'D' -> Pos   0 (-1)
            'L' -> Pos (-1)  0
            'R' -> Pos   1   0

withLen :: Int -> Rope
withLen n = replicate n (Pos 0 0)

visited :: Rope -> [Offset] -> Set Offset
visited _ [] = Set.empty
visited rope (m : ms) = let moved = move m rope in
    Set.singleton (last moved) `Set.union` visited moved ms

main = solution 9 $ \input ->
    let motions = parseOffsets input
      in pmap Set.size
         ( visited (withLen 2 ) motions
         , visited (withLen 10) motions )
