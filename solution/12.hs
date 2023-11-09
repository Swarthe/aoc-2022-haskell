import Lib

import Data.Set (Set, union, notMember)
import qualified Data.Set as Set

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Data.List (elemIndices)

-- TODO: remove unnecessary type sigs
-- TODO: rename mapPair to `pmap` (doesnt operator on list)
-- TODO: reexport from Lib
import Control.Monad.State

data HeightMap = HeightMap
    { heights :: Map Pos Int
    , h, w :: Int }

data Journey = Journey
    { start, end :: Pos
    , stepsMade :: Int }

-- TODO: move to Lib (also replacing Coord for 09)
--       also for instance below
data Pos = Pos { x, y :: Int }
  deriving (Eq, Ord)

instance Num Pos where
    (Pos x y) + (Pos x' y') = Pos (x + x') (y + y')
    (Pos x y) - (Pos x' y') = Pos (x - x') (y - y')
    abs (Pos x y) = Pos (abs x) (abs y)
    (*) = undefined; signum = undefined; fromInteger = undefined

nextSteps :: HeightMap -> Pos -> [Pos]
nextSteps (HeightMap heights h w) pos =
    [Pos 1 0, Pos 0 1, Pos (-1) 0, Pos 0 (-1)]
        & map (+ pos)
        & filter inBounds
        & filter (isSteppable (heights ! pos) . (heights !))
  where isSteppable h i = h + 1 >= i
        inBounds p@(Pos x y) = abs p == p && x < w && y < h

parseHeightMap = lines >>> \ls ->
    let (h, w) = (length ls, length (head ls))
     in concat ls & zipWith (\i c -> (ixToPos i w, parseHeight c)) [0..]
                  & \pHeights -> HeightMap (Map.fromList pHeights) h w
  where ixToPos i w = Pos (i `rem` w) (i `div` w)
        parseHeight 'S' = ord 'a'
        parseHeight 'E' = ord 'z'
        parseHeight  c  = ord c

parseJourney = lines >>> \ls ->
    Journey { start = elemPos 'S' ls
            , end = elemPos 'E' ls
            , stepsMade = 0 }
  where elemPos e rows = head
            [Pos x y | (y, cols) <- zip [0..] rows,
                        x        <- elemIndices e cols]

travelShortest :: HeightMap -> Journey -> Journey
travelShortest hmap journey@(Journey start end _) =
    go (Set.singleton start) [journey]
  where go visited (j@(Journey start _ stepsMade) : js)
            | start == end = j
            | otherwise = go visited' (js ++ next)
          where steps = nextSteps hmap start
                    & filter (`notMember` visited)
                next = steps
                    & map (\s -> Journey s end (stepsMade + 1))
                visited' = visited `union` Set.fromList steps

main = solution 12 $ \input -> do
    let hmap = parseHeightMap input
        journey = parseJourney input
        shortest = travelShortest hmap journey
     in ( stepsMade shortest
        , "todo" )
