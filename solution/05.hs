import Lib

type Crate = Char

-- crates are arranged top to bottom
type Stack = [Crate]

data Proc = Proc { num, from, to :: Int }

-- describes a crane that lifts a certain number of crates off a stack
type Lifter = (Int -> Stack -> [Crate])

parseStacks :: String -> [Stack]
parseStacks = lines
    >>> take 8
    >>> map compact
    >>> transpose
    >>> map (filter isAlpha)
  where compact s = [c | (i, c) <- zip [0..] s,
                         (i - 1) `rem` 4 == 0]

parseProcs :: String -> [Proc]
parseProcs = lines >>> drop 10 >>>
    map (\l -> let (a, b, c) = (nextNum l, nextNum a, nextNum b) in
               Proc { num = int a, from = int b - 1, to = int c - 1 })
  where nextNum s | isDigit (head s) = nextNum (dropWhile isDigit s)
                  | otherwise = dropWhile (not . isDigit) s
        int = read . takeWhile isDigit

rearrange :: Lifter -> [Proc] -> [Stack] -> [Stack]
rearrange _    []       stacks = stacks
rearrange lift (p : ps) stacks = rearrange lift ps (applyProc p)
  where applyProc (Proc num from to) =
            let fromStack = stacks !! from
                toStack   = stacks !! to
            in stacks & set from (drop num fromStack)
                      & set to (lift num fromStack ++ toStack)
        set i x xs = take i xs ++ (x : drop (i + 1) xs)

rearrange9000 :: [Proc] -> [Stack] -> [Stack]
rearrange9000 = rearrange (\i s -> reverse $ take i s)

rearrange9001 :: [Proc] -> [Stack] -> [Stack]
rearrange9001 = rearrange take

topCrates :: [Stack] -> [Crate]
topCrates = map head

main = solution 5 $ \input ->
    let stacks = parseStacks input
        procs = parseProcs input
     in mapPair topCrates
            ( rearrange9000 procs stacks
            , rearrange9001 procs stacks )
