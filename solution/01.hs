import Lib

parseCalorieSums :: String -> [Int]
parseCalorieSums = lines
    >>> splitOn ""
    >>> map (sum . map read)
    >>> sort

last3 [a, b, c] = [a, b, c]
last3 (_ : xs) = last3 xs

main = do
    calSums <- parseCalorieSums <$> readInput 1

    print $ last calSums
    print $ last3 calSums & sum
