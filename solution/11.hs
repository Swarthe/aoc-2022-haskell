import Lib

import Control.Monad.State

import Data.Sequence (Seq, index, update, adjust)
import qualified Data.Sequence as Seq

type MonkeyIx = Int

data Op = Mul Int | Add Int | Square

data Monkey = Monkey
    { ix :: MonkeyIx
    , itemWorries :: [Int]
    , worryOp :: Op
    , testDivBy :: Int
    , targetIfTrue :: MonkeyIx
    , targetIfFalse :: MonkeyIx
    , inspections :: Int }

runTurn :: Monkey -> State (Seq Monkey) ()
runTurn monkey = do
    let items = itemWorries monkey

    forM_ items $ \item ->
        let newItem = apply (worryOp monkey) item `div` 3
         in modify $ flip adjust (targetIxFor newItem) $
                \m -> m { itemWorries = itemWorries m ++ [newItem] }

    modify $ update (ix monkey) monkey
        { inspections = inspections monkey + length items
        , itemWorries = [] }
  where targetIxFor worry = monkey &
            if worry `rem` testDivBy monkey == 0
            then targetIfTrue else targetIfFalse
        apply o worry = case o of
            Mul i -> worry * i
            Add i -> worry + i
            Square -> worry * worry

parseMonkeys :: String -> Seq Monkey
parseMonkeys = Seq.fromList . map parseMonkey . splitOn "" . lines
  where parseMonkey (a : b : c : d : e : f : _) =
            Monkey { ix = readLastInt (init a)
                   , itemWorries = parseItems b
                   , worryOp = parseOp c
                   , testDivBy = readLastInt d
                   , targetIfTrue = readLastInt e
                   , targetIfFalse = readLastInt f
                   , inspections = 0 }
        parseOp l = let c : _ : s = drop 23 l in
            case c of '*' | s == "old" -> Square
                          | otherwise -> Mul (read s)
                      '+' -> Add (read s)
        parseItems = map read . splitOn ',' . drop 18
        readLastInt = read . last . words

runRounds :: Seq Monkey -> [Seq Monkey]
runRounds = iterate (execState runRound)
  where runRound = do
            monkeyCount <- gets Seq.length

            forM_ [0 .. monkeyCount - 1] $ \i ->
                gets (`index` i) >>= runTurn

monkeyBusiness :: Seq Monkey -> Int
monkeyBusiness monkeys = inspections a * inspections b
  where ord = Seq.reverse (Seq.sortOn inspections monkeys)
        (a, b) = (ord `index` 0, ord `index` 1)

main = solution 11 $ \input ->
    let rounds = runRounds (parseMonkeys input)
     in ( monkeyBusiness (rounds !! 20)
        , "TODO" )
