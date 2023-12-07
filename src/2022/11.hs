import Lib

import Data.Sequence (Seq, index, update, adjust)
import qualified Data.Sequence as Seq

type MonkeyIx = Int

type Handler = Monkey -> Int -> Int

type Reduction = Int

data Op = Mul Int | Add Int | Square

data Monkey = Monkey
    { ix :: MonkeyIx
    , itemWorries :: [Int]
    , worryOp :: Op
    , testDivBy :: Int
    , targetIfTrue :: MonkeyIx
    , targetIfFalse :: MonkeyIx
    , inspections :: Int }

runTurn :: Handler -> Monkey -> State (Seq Monkey) ()
runTurn handler monkey = do
    let items = itemWorries monkey

    forM_ items $ \item ->
        let newItem = handler monkey item
         in modify $ flip adjust (targetIxFor newItem) $
                \m -> m { itemWorries = itemWorries m ++ [newItem] }

    modify $ update (ix monkey) monkey
        { inspections = inspections monkey + length items
        , itemWorries = [] }
  where targetIxFor worry = monkey &
            if worry `rem` testDivBy monkey == 0
            then targetIfTrue else targetIfFalse

parseMonkeys = Seq.fromList . map parseMonkey . splitOn "" . lines
  where parseMonkey (a : b : c : d : e : f : _) =
            Monkey { ix = readLast (init a)
                   , itemWorries = parseItems b
                   , worryOp = parseOp c
                   , testDivBy = readLast d
                   , targetIfTrue = readLast e
                   , targetIfFalse = readLast f
                   , inspections = 0 }
        parseOp l = let c : _ : s = drop 23 l in
            case c of '*' | s == "old" -> Square
                          | otherwise -> Mul (read s)
                      '+' -> Add (read s)
        parseItems = map read . splitOn ',' . drop 18
        readLast = read . last . words

monkeyReduction :: Seq Monkey -> Reduction
monkeyReduction = product . fmap testDivBy

handlerWith :: Reduction -> Handler
handlerWith reduction monkey = (`rem` reduction)
    >>> case worryOp monkey of
            Mul i -> (* i)
            Add i -> (+ i)
            Square -> (^ 2)

runRounds :: Handler -> Seq Monkey -> [Seq Monkey]
runRounds handler = iterate (execState runRound)
  where runRound = do
            monkeyCount <- gets Seq.length

            forM_ [0 .. monkeyCount - 1] $ \i ->
                gets (`index` i) >>= runTurn handler
        applyWith reduction monkey = (`rem` reduction)
            >>> case worryOp monkey of
                    Mul i -> (* i)
                    Add i -> (+ i)
                    Square -> (^ 2)

monkeyBusiness :: Seq Monkey -> Int
monkeyBusiness monkeys = inspections a * inspections b
  where ord = Seq.reverse (Seq.sortOn inspections monkeys)
        (a, b) = (ord `index` 0, ord `index` 1)

main = solution 11 $ \input ->
    let monkeys = parseMonkeys input
        reduction = monkeyReduction monkeys
        handler = handlerWith reduction
        rounds1 = runRounds (((`div` 3) .) . handler) monkeys
        rounds2 = runRounds handler monkeys
     in ( monkeyBusiness (rounds1 !! 20)
        , monkeyBusiness (rounds2 !! 10000) )
