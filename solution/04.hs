import Lib

type SectionID = Int

type Elf = (SectionID, SectionID)

type ElfPairs = [(Elf, Elf)]

parseElves :: String -> ElfPairs
parseElves = lines >>> map (splitOnFirst ',' >>> mapPair parseElf)
  where parseElf = splitOnFirst '-' >>> mapPair read

countContaining :: ElfPairs -> Int
countContaining = filter (\(a, b) -> (fst a <= fst b && snd a >= snd b)
                                  || (fst a >= fst b && snd a <= snd b))
                  >>> length

countOverlapping :: ElfPairs -> Int
countOverlapping = filter (\(a, b) -> not ((fst a < fst b && snd a < fst b)
                                        || (fst a > snd b && snd a > snd b)))
                   >>> length

main = do
    elves <- parseElves <$> readInput 4

    print $ countContaining  elves
    print $ countOverlapping elves
