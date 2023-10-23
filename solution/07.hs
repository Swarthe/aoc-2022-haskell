import Lib

import Data.List (groupBy, minimumBy)

data Cmd = Cd { dir :: String }
         | Ls { output :: [String] }

data Item = Dir { dpath :: [String] }
          | File { fpath :: [String], size :: Int }

type ItemSize = (Item, Int)

parseCmds :: String -> [Cmd]
parseCmds = conv . group . lines
  where group = groupBy $ \a b -> case head a of
            '$' -> head b == '$'
            _   -> head b /= '$'
        conv gs = case gs of
            (cmds : outs : gs) ->
                map (conv1 outs) cmds ++ conv gs
            [] -> []
        conv1 outs c = case take 4 c of
            "$ cd" -> Cd { dir = drop 5 c }
            "$ ls" -> Ls { output = outs }

parseItems :: String -> [Item]
parseItems = (:) (Dir ["/"]) . go [] . parseCmds
  where go curPath (Cd dir : cmds) =
            let newPath = case dir of
                 "/"  -> ["/"]
                 ".." -> init curPath
                 dir  -> curPath ++ [dir]
            in go newPath cmds
        go curPath (Ls output : cmds) =
            map (parseItem curPath) output ++ go curPath cmds
        go _ [] = []

        parseItem curPath raw = case take 3 raw of
            "dir" -> Dir { dpath = curPath ++ [drop 4 raw] }
            _     -> let i = elemIx ' ' raw in
                     File { fpath = curPath ++ [drop (i + 1) raw]
                          , size = read $ take i raw }

dirs :: [Item] -> [Item]
dirs = filter $ \item -> case item of
    Dir _ -> True
    File _ _ -> False

dirSizes :: [Item] -> [ItemSize]
dirSizes items = dirs items
    & map (\d -> (d, size items d))
  where size items item = case item of
            Dir dir -> items
                & filter (isChild dir . path)
                & map (size items)
                & sum
            File _ size -> size

        isChild dir child =
            dir `isPrefixOf` child
         && length child == length dir + 1

        path item = case item of
            Dir dpath -> dpath
            File fpath _ -> fpath

findDirToShrink :: Int -> [ItemSize] -> ItemSize
findDirToShrink size itemSizes =
    filter ((>= delSize) . snd) itemSizes
        & minimumBy (\a b -> compare (snd a) (snd b))
  where totalSize = snd (head itemSizes)
        delSize = totalSize - size

main = solvePuzzle 7 $ \input ->
    let ds = dirSizes (parseItems input)
     in ( ds & map snd
             & filter (<= 100000)
             & sum
        , ds & findDirToShrink 40000000
             & snd )
