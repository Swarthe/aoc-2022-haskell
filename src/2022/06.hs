import Lib

findMarkerIx :: Int -> String -> Int
findMarkerIx n = windows n
    >>> findIx (\w -> length w == length (nub w))
    >>> (+ n)   -- past the marker

main = solution 6 $ \input ->
    ( findMarkerIx 4  input
    , findMarkerIx 14 input )
