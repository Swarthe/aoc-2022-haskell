import Lib

type Item = Char

data Rucksack = Rucksack [Item] [Item]

data Group = Group Rucksack Rucksack Rucksack

prio :: Item -> Int
prio i | isLower i = ord i - 96
       | otherwise = ord i - 38

badge :: Group -> Item
badge (Group a b c) = toList a
    & filter (`elem` toList b)
    & filter (`elem` toList c)
    & head
  where toList (Rucksack a b) = a ++ b

parseRucksacks :: String -> [Rucksack]
parseRucksacks = lines >>> map (uncurry Rucksack . splitHalf)
  where splitHalf l = splitAt (length l `div` 2) l

parseGroups :: String -> [Group]
parseGroups = parseRucksacks
    >>> subGroup 3
    >>> map (\[a, b, c] -> Group a b c)
  where subGroup _ [] = []
        subGroup n l = take n l : subGroup n (drop n l)

totalDupPrio :: [Rucksack] -> Int
totalDupPrio = map (prio . dupItem) >>> sum
  where dupItem (Rucksack a b) = a & filter (`elem` b)
                                   & head

totalBadgePrio :: [Group] -> Int
totalBadgePrio = map (prio . badge) >>> sum

main = do
    input <- readInput 3

    print $ totalDupPrio   (parseRucksacks input)
    print $ totalBadgePrio (parseGroups    input)
