import Lib

parseCalorieSums :: String -> [Int]
parseCalorieSums = lines
    >>> splitOn ""
    >>> map (sum . map read)
    >>> sort

last3 [a, b, c] = [a, b, c]
last3 (_ : xs) = last3 xs

main = solution 1 $ \input ->
    let calSums = parseCalorieSums input
     in ( last calSums
        , sum $ last3 calSums )
