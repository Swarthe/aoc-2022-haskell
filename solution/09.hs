import Lib

import Data.Set (Set)
import qualified Data.Set as Set

type Rope = [Coord]

type Coord = (Int, Int)

-- values within [-1 .. 1]
type Offset = Coord

move :: Offset -> Rope -> Rope
move offset (h : ts) =
    scanl follow (h +@ offset) ts
  where follow h t
            | let (x, y) = h -@ t in
              all (inRange (-1, 1)) [x, y] = t          -- adjacency
            | otherwise = h +@ case h -@ t of
                (x, y) | (abs x, abs y) == (2, 2)       -- corners
                       -> (- signum x, - signum y)
                (x, _) | x > 1  -> (-1,  0)             -- edges
                       | x < -1 -> ( 1,  0)
                (_, y) | y > 1  -> ( 0, -1)
                       | y < -1 -> ( 0,  1)
        (x, y) +@ (x', y') = (x + x', y + y')
        (x, y) -@ (x', y') = (x - x', y - y')

parseOffsets = concat . mapLines parseGroup
  where parseGroup (dir : _ : n) =
            replicate (read n) $ case dir of
                'U' -> ( 0,  1)
                'D' -> ( 0, -1)
                'L' -> (-1,  0)
                'R' -> ( 1,  0)

withLen :: Int -> Rope
withLen n = replicate n (0, 0)

visited :: Rope -> [Offset] -> Set Offset
visited _ [] = Set.empty
visited rope (m : ms) = let moved = move m rope in
    Set.singleton (last moved) `Set.union` visited moved ms

main = solution 9 $ \input ->
    let motions = parseOffsets input
      in mapPair Set.size
             ( visited (withLen 2) motions
             , visited (withLen 10) motions )
