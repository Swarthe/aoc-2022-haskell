import Lib

import Data.Set (Set)
import qualified Data.Set as Set

data Rope = Rope { h, t :: Pos }

data Pos = Pos { x, y :: Int } deriving (Eq, Ord)

data Motion = U | D | L | R deriving (Read, Eq)

instance Num Pos where
    Pos x y + Pos x' y' = Pos (x + x') (y + y')
    Pos x y - Pos x' y' = Pos (x - x') (y - y')

-- how many rotations clockwise to normalise with `rotate`
rotationCoeff :: Rope -> Int
rotationCoeff (Rope h t)
    | ix `elem` [2, 5] = 3
    | ix `elem` [0, 1] = 2
    | ix `elem` [3, 6] = 1
    | otherwise = 0                     -- 6 7 8
  where ix = let Pos x y = t - h in     -- 3 4 5
            (x + 1) + (y + 1) * 3       -- 0 1 2

-- rotate clockwise a given number of times
--
-- B A B  |          A B |
-- A O A -O-   =>    O   O-
-- B A B  |
rotate :: Int -> (Rope, Motion) -> (Rope, Motion)
rotate 0 ropeMotion = ropeMotion
rotate coeff (Rope h t, motion) = rotate (coeff - 1) (r, m)
  where m = case motion of
            U -> R; R -> D; D -> L; L -> U
        r = Rope h (rot (t - h) + h)
        rot (Pos x y) = Pos y (-x)

unrotate :: Int -> Rope -> Rope
unrotate 0 rope = rope
unrotate coeff (Rope h t) = unrotate (coeff - 1) r
  where r = Rope h (unrot (t - h) + h)
        unrot (Pos x y) = Pos (-y) x

moveRope :: Motion -> Rope -> Rope
moveRope motion rope =
    Rope { h = movePos motion (h rope)
         , t = t rope' }
  where movePos motion (Pos x y) = case motion of
            U -> Pos x (y + 1)
            D -> Pos x (y - 1)
            L -> Pos (x - 1) y
            R -> Pos (x + 1) y
        rope' =
            let coeff = rotationCoeff rope
                (ropeR, motionR) = rotate coeff (rope, motion)
                Rope hR tR = ropeR
                tR' = case tR - hR of
                   Pos 0 0 -> tR
                   Pos 0 1 | motionR == D -> movePos D tR
                           | otherwise -> tR
                   Pos 1 1 | motionR `elem` [L, D] -> hR
                           | otherwise -> tR
            -- unrotate around an unmoved reference point
            in unrotate coeff (Rope hR tR')

parseMotions :: String -> [Motion]
parseMotions = concat . mapLines parseMotion
  where parseMotion (dir : _ : count) =
            replicate (read count) (read [dir])

-- positions visited by the tail
visitedPositions :: [Motion] -> Set Pos
visitedPositions = go (Rope orig orig)
  where go rope (m : ms) = let moved = moveRope m rope
            in Set.singleton (t moved) `Set.union` go moved ms
        go _ [] = Set.empty

        orig = Pos 0 0

main = do
    motions <- parseMotions <$> readInput 9

    print $ Set.size (visitedPositions motions)
