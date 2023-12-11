import Lib

import Data.Set (Set, union, notMember)
import qualified Data.Set as Set

import qualified Data.Map as Map

import Data.List (elemIndices)

data Journey = Journey { pos :: Pos, stepsMade :: Int }

type HeightMap = Grid Int

nextSteps :: HeightMap -> Pos -> [Pos]
nextSteps hmap@(Grid _ h w) pos = neighbours pos hmap
    & filter (isSteppable (hmap ! pos) . (hmap !))
  where isSteppable h i = h + 1 >= i

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

parseHeightMap :: String -> HeightMap
parseHeightMap = fromList . mapLines (map parseHeight)
  where parseHeight 'S' = ord 'a'
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
stepsFromLow hmap@(Grid heights _ _) end = lows
    & map (`Journey` 0)
    & \js -> travelSteps hmap (Set.fromList lows) js end
  where lows = Map.keys $ Map.filter (== ord 'a') heights

main = solution 12 $ \input -> do
    let hmap = parseHeightMap input
        (start, end) = parseStartEnd input
     in ( steps hmap start end
        , stepsFromLow hmap end )
