import Lib

data Move = Rock | Paper | Scissors

data Result = Victory | Loss | Draw

data Game = Game Move Move

data Pred = Pred Move Result

moveFromChar c | c `elem` "AX" = Rock
               | c `elem` "BY" = Paper
               | c `elem` "CZ" = Scissors

moveFromInt i = case i of
    0 -> Rock
    1 -> Paper
    2 -> Scissors

moveToInt m = case m of
    Rock -> 0
    Paper -> 1
    Scissors -> 2

play :: Game -> Result
play (Game a b) = case (3 + i - j) `rem` 3 of
    0 -> Draw
    1 -> Victory
    2 -> Loss
  where (i, j) = (moveToInt a, moveToInt b)

moveForPred (Pred m r) = moveFromInt $ case r of
    Victory -> (i + 1) `mod` 3
    Loss -> (i - 1) `mod` 3
    Draw -> i
  where i = moveToInt m

parseWith :: (Char -> Char -> a) -> String -> [a]
parseWith f = mapLines (\(a : _ : b : _) -> f a b)

parseGames = parseWith $ \c d ->
    Game (moveFromChar d) (moveFromChar c)

parsePredGames = map predict . parsePreds
  where parsePreds = parseWith $ \c d ->
            Pred (moveFromChar c) (resultFromChar d)
        predict p = let Pred m _ = p in
            Game (moveForPred p) m

        resultFromChar c = case c of
            'X' -> Loss
            'Y' -> Draw
            'Z' -> Victory

computeScore :: [Game] -> Int
computeScore games = sum $
    zipWith (\(Game m _) r -> moveToScore m + resultToScore r)
            games results
  where moveToScore =
            (+ 1) . moveToInt
        resultToScore r = case r of
            Draw -> 3
            Victory -> 6
            Loss -> 0
        results =
            map play games

main = solution 2 $ \input ->
    mapPair computeScore
        ( parseGames     input
        , parsePredGames input )
