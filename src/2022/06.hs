import Lib

findMarkerIx :: Int -> String -> Int
findMarkerIx n = windows
    >>> findIx (\w -> length w == length (nub w))
    >>> (+ n)   -- past the marker
  where
    windows l | length l >= n = take n l : windows (tail l)
              | otherwise     = []

main = solution 6 $ \input ->
    ( findMarkerIx 4  input
    , findMarkerIx 14 input )
