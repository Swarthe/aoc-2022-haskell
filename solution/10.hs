import Lib

import Prelude hiding (cycle)

import Control.Monad.State

import Debug.Trace (traceShowId)

data Instr = NoOp | AddX Int

data Cpu = Cpu { cycle, x :: Int } deriving Show

parseInstrs :: String -> [Instr]
parseInstrs = concat . mapLines parseInstr
  where parseInstr l = let (a, b) = pierceAt 4 l in
            case a of "noop" -> [NoOp]
                      "addx" -> [NoOp, AddX (read b)]   -- addx takes 2 cycles

exec :: Instr -> Cpu -> Cpu
exec NoOp (Cpu cycle x) = Cpu (cycle + 1) x
exec (AddX i) (Cpu cycle x) = Cpu (cycle + 1) (x + i)

sigStrength :: Cpu -> Maybe Int
sigStrength (Cpu cycle x)
    | (cycle - 20) `rem` 40 == 0 = Just (cycle * x)
    | otherwise = Nothing

firstSigStrengths :: Int -> [Instr] -> [Int]
firstSigStrengths n instrs =
    evalState go (Cpu 1 1, instrs)
        & take n
  where go :: State (Cpu, [Instr]) [Int]
        go = do
            (cpu, instrs) <- get

            sigs <- case instrs of
                i : is -> put (exec i cpu, is) >> go
                [] -> return []

            return (toList (sigStrength cpu) ++ sigs)

main = solvePuzzle 10 $ \input ->
    let instrs = parseInstrs input
     in ( sum (6 `firstSigStrengths` instrs)
        , "" )
