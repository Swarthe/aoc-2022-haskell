import Lib

type SectionID = Int

type Elf = (SectionID, SectionID)

parseElves :: String -> [(Elf, Elf)]
parseElves = mapLines (pmap parseElf . splitOnFirst ',')
  where parseElf = pmap read . splitOnFirst '-'

countContaining :: [(Elf, Elf)] -> Int
countContaining = count
    $ \(a, b) -> (fst a <= fst b && snd a >= snd b)
              || (fst a >= fst b && snd a <= snd b)

countOverlapping :: [(Elf, Elf)] -> Int
countOverlapping = count
    $ \(a, b) -> not ((fst a < fst b && snd a < fst b)
                   || (fst a > snd b && snd a > snd b))

main = solution 4 $ \input ->
    let elves = parseElves input
     in ( countContaining  elves
        , countOverlapping elves )
