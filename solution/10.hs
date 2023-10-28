import Lib

import Prelude hiding (cycle)

import Data.Default
import Control.Monad.State

data Instr = NoOp | AddX Int

data Cpu = Cpu { cycle, x :: Int } deriving Show

newtype Crt = Crt String

instance Default Cpu where
    def = Cpu 1 1

instance Show Crt where
    show (Crt s) = s    -- to preserve newlines

exec :: Instr -> Cpu -> Cpu
exec NoOp (Cpu cycle x) = Cpu (cycle + 1) x
exec (AddX i) (Cpu cycle x) = Cpu (cycle + 1) (x + i)

parseInstrs :: String -> [Instr]
parseInstrs = concat . mapLines parseInstr
  where parseInstr l = let (a, b) = pierceAt 4 l in
            case a of "noop" -> [NoOp]
                      "addx" -> [NoOp, AddX (read b)]   -- addx takes 2 cycles

sumSigStrengths :: Int -> [Instr] -> Int
sumSigStrengths n instrs = sum . take n
    $ evalState (strengths instrs) def
  where strengths :: [Instr] -> State Cpu [Int]
        strengths instrs = do
            cpu <- get

            sigs <- case instrs of
                i : is -> put (exec i cpu)
                    >> strengths is
                [] -> return []

            return (toList (strength cpu) ++ sigs)
        strength (Cpu cycle x)
            | (cycle - 20) `rem` 40 == 0 = Just (cycle * x)
            | otherwise = Nothing

renderCrt :: [Instr] -> Crt
renderCrt instrs = evalState (compute instrs) def
    & splitEvery 40
    & intercalate "\n"
    & Crt
  where compute :: [Instr] -> State Cpu String
        compute instrs = do
            cpu@(Cpu cycle x) <- get

            let sprite = [x - 1 .. x + 1]
                cursor = (cycle - 1) `rem` 40
                pixel = if cursor `elem` sprite
                    then '#' else '.'

            case instrs of
                i : is -> put (exec i cpu)
                    >> (pixel :) <$> compute is
                [] -> return []

        splitEvery _ [] = []
        splitEvery n xs = let (ys, zs) = splitAt n xs
            in ys : splitEvery n zs

main = solution 10 $ \input ->
    let instrs = parseInstrs input
     in ( sumSigStrengths 6 instrs
        , renderCrt instrs )
