import Lib

findMarkerIx :: Int -> String -> Int
findMarkerIx n = windows
    >>> findIx (\w -> length w == length (nub w))
    >>> (+ n)   -- past the marker
  where windows l | length l >= n = take n l : windows (tail l)
                  | otherwise     = []

main = do
    input <- readInput 6

    print $ findMarkerIx 4  input
    print $ findMarkerIx 14 input
