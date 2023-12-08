import Lib

type Card = Int
type Strength = Int

data Play = Play { hand :: [Card], bid :: Int }

strength :: [Card] -> Strength
strength h
    | areEqual 5 h = 6
    | areEqual 4 h = 5
    | areEqual 3 h = if areEqual 2 h
           then 4 else 3
    | areEqual 2 h = if length (nub h) == 3
           then 2 else 1
    | otherwise = 0
  where areEqual n l = any (\x -> n == count (== x) l) l

-- first determines rank, second tiebreaks and is kept
sortPlaysWith :: (Play -> ([Card], Play)) -> [Play] -> [Play]
sortPlaysWith f = map (first strength . f)
    >>> sortOn fst
    >>> groupBy (\(a, _) (b, _) -> a == b)
    >>> concatMap (sortOn hand . map snd)

sortPlays = sortPlaysWith (\p -> (hand p, p))

sortPlaysJoker = sortPlaysWith $ \p@(Play h _) ->
    (upgradeJokers h, p { hand = nullifyJokers h })
  where
    nullifyJokers =
        map (\c -> if c == 10 then 0 else c)
    upgradeJokers h =
        let m = mode (filter (/= 10) h)
         in map (\c -> if c == 10 then m else c) h
    mode hand = fst $ foldr
        (\c (c', n') -> let n = count (== c) hand in
            if n > n' then (c, n) else (c', n'))
        (0, 0) hand

parsePlays = mapLines $ \l ->
    let [a, b] = splitOn ' ' l
     in Play { hand = map (`elemIx` "_23456789TJQKA") a
             , bid = read b }

-- plays must be sorted by rank
totalWinnings = zip [1..]
    >>> map (\(i, Play _ bid) -> bid * i)
    >>> sum

main = solution 7 $ \input ->
    let plays = parsePlays input
     in pmap totalWinnings
        ( sortPlays      plays
        , sortPlaysJoker plays )
