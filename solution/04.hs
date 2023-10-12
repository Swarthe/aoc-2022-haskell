import Lib

type SectionID = Int

type Elf = (SectionID, SectionID)

parseElves :: String -> [(Elf, Elf)]
parseElves = mapLines (splitOnFirst ',' >>> mapPair parseElf)
  where parseElf = splitOnFirst '-' >>> mapPair read

countContaining :: [(Elf, Elf)] -> Int
countContaining = filter (\(a, b) -> (fst a <= fst b && snd a >= snd b)
                                  || (fst a >= fst b && snd a <= snd b))
                  >>> length

countOverlapping :: [(Elf, Elf)] -> Int
countOverlapping = filter (\(a, b) -> not ((fst a < fst b && snd a < fst b)
                                        || (fst a > snd b && snd a > snd b)))
                   >>> length

main = do
    elves <- parseElves <$> readInput 4

    print $ countContaining  elves
    print $ countOverlapping elves
