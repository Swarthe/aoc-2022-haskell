import Lib

import Data.Set (Set, union, notMember)
import qualified Data.Set as Set

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Data.List (elemIndices)

data HeightMap = HeightMap
    { heights :: Map Pos Int
    , h, w :: Int }

data Journey = Journey
    { pos :: Pos, stepsMade :: Int }

nextSteps :: HeightMap -> Pos -> [Pos]
nextSteps (HeightMap heights h w) pos =
    [Pos 1 0, Pos 0 1, Pos (-1) 0, Pos 0 (-1)]
        & map (+ pos)
        & filter inBounds
        & filter (isSteppable (heights ! pos) . (heights !))
  where isSteppable h i = h + 1 >= i
        inBounds p@(Pos x y) = abs p == p && x < w && y < h

-- BFS
travelSteps :: HeightMap -> Set Pos -> [Journey] -> Pos -> Int
travelSteps hmap visited (Journey pos stepsMade : js) end
    | pos == end = stepsMade
    | otherwise = travelSteps hmap visited' (js ++ next) end
  where steps = nextSteps hmap pos
            & filter (`notMember` visited)
        next = steps
            & map (\p -> Journey p (stepsMade + 1))
        visited' = visited `union` Set.fromList steps

parseHeightMap = lines >>> \ls ->
    let (h, w) = (length ls, length (head ls))
     in concat ls & zipWith (\i c -> (ixToPos i w, parseHeight c)) [0..]
                  & \pHeights -> HeightMap (Map.fromList pHeights) h w
  where ixToPos i w = Pos (i `rem` w) (i `div` w)
        parseHeight 'S' = ord 'a'
        parseHeight 'E' = ord 'z'
        parseHeight  c  = ord c

parseStartEnd = lines >>> \ls ->
    (elemPos 'S' ls, elemPos 'E' ls)
  where
    elemPos e rows = head
        [Pos x y | (y, cols) <- zip [0..] rows,
                    x        <- elemIndices e cols]

steps :: HeightMap -> Pos -> Pos -> Int
steps hmap start = travelSteps hmap (Set.singleton start) js
  where js = [Journey start 0]

stepsFromLow :: HeightMap -> Pos -> Int
stepsFromLow hmap@(HeightMap heights _ _) end = lows
    & map (`Journey` 0)
    & \js -> travelSteps hmap (Set.fromList lows) js end
  where lows = Map.keys $ Map.filter (== ord 'a') heights

main = solution 12 $ \input -> do
    let hmap = parseHeightMap input
        (start, end) = parseStartEnd input
     in ( steps  hmap start end
        , stepsFromLow hmap end )
