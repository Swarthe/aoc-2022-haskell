import Lib

import Data.Map (Map)
import qualified Data.Map as Map

data Journey = Journey { cur, prev :: Pos }

type Tile = Char

-- TODO: explain
offsetMap :: Map (Pos, Pos) Tile
offsetMap = Map.fromList
    [ ((Pos 0   1,  Pos   0 (-1)), '|')
    , ((Pos 1   0,  Pos (-1)  0 ), '-')
    , ((Pos 0 (-1), Pos   1   0 ), 'L')
    , ((Pos 0 (-1), Pos (-1)  0 ), 'J')
    , ((Pos 0   1,  Pos (-1)  0 ), '7')
    , ((Pos 0   1,  Pos   1   0 ), 'F') ]

isVert p ts = case ts ! p of
    '-' -> False
    _   -> True

determineStart :: Grid Tile -> Grid Tile
determineStart ts = undefined
  where
    s' = offsetMap Map.! sLinkOffsets
    sLinkOffsets = neighbours s ts
        & map (\n -> (n, links n ts))
        & filter (\(_, (a, b)) -> a == s || b == s)
        & \[(n, _), (o, _)] -> (n, o)
    s = elemPos 'S' ts

links :: Pos -> Grid Tile -> (Pos, Pos)
links p ts = pmap (p +) $ case ts ! p of
    '|' -> (Pos 0   1,  Pos   0 (-1))
    '-' -> (Pos 1   0,  Pos (-1)  0 )
    'L' -> (Pos 0 (-1), Pos   1   0 )
    'J' -> (Pos 0 (-1), Pos (-1)  0 )
    '7' -> (Pos 0   1,  Pos (-1)  0 )
    'F' -> (Pos 0   1,  Pos   1   0 )

-- starts at 'S' and stops right before
-- somewhat like a foldr on the pipe loop
travel :: Default a => (Pos -> a -> a) -> Grid Tile -> a
travel f ts = f start $ go
    Journey { cur = fst $ links start ts
            , prev = start }
  where
    go (Journey cur prev)
        | cur == start = def
        | otherwise =
            let next = pick (/= prev) (links cur ts)
             in f cur $ go (Journey next cur)

    pick f (a, b) = if f a then a else b
    start = elemPos 'S' ts

extremum :: Grid Tile -> Int
extremum = (`div` 2) . travel (const (+ 1))

enclosed :: Grid Tile -> Int
enclosed ts = sum (map enclosedInRow pipeRows)
  where
    pipeRows = travel (:) ts
        & sortOn y
        & groupBy ((==) `on` y)
        & map (sortOn x)

    enclosedInRow = filter (`isVert` ts)
        >>> windows 2
        >>> map (\ab -> if isPeak ab then fuse ab else ab)
        >>> dropSnds                            -- ignore inner meanders
        >>> map (\[a, b] -> x b - x a - 1)      -- distance = enclosed
        >>> sum

    fuse [a, b] = [a, b { x = x a + 1 }]

    isPeak ab = let [x, y] = map (ts !) ab in
        (x, y) == ('F', '7') || (x, y) == ('L', 'J')

    dropSnds (a : b : xs) = a : dropSnds xs
    dropSnds [a] = [a]
    dropSnds [] = []

main = solution 10 $ \input -> do
    let tiles = fromList (lines input)
     in ( extremum tiles
        , enclosed tiles )
