import Lib

import qualified Data.Map as Map

data Journey = Journey { cur, prev :: Pos }

-- TODO: optimise `fromList` in `Lib` to simple version

-- doesn't work for start 'S'
links :: Pos -> Grid Char -> (Pos, Pos)
links p ts = pmap (p +) $ case ts ! p of
    '|' -> (Pos 0   1,  Pos   0 (-1))
    '-' -> (Pos 1   0,  Pos (-1)  0 )
    'L' -> (Pos 0 (-1), Pos   1   0 )
    'J' -> (Pos 0 (-1), Pos (-1)  0 )
    '7' -> (Pos 0   1,  Pos (-1)  0 )
    'F' -> (Pos 0   1,  Pos   1   0 )

startLink :: Pos -> Grid Char -> Pos
startLink p ts = neighbours p ts
    & map (\n -> (n, links n ts))
    & filter (\(_, (a, b)) -> a == p || b == p)
    & fst . head

steps :: Grid Char -> Int
steps ts = 1 + evalState toStart
    Journey { cur = startLink start ts
            , prev = start }
  where
    toStart = do
        Journey cur prev <- get

        if cur == start then
            return 0
        else do
            let next = pick (/= prev) (links cur ts)
            put (Journey next cur)
            (1 +) <$> toStart

    pick f (a, b) = if f a then a else b
    start = elemPos 'S' ts

main = solution 10 $ \input -> do
    let tiles = fromList (lines input)
     in ( steps tiles `div` 2
        , "" )
