module Lib
  ( solution, splitOn, splitOnFirst, pierceAt, elemIx, findIx, mapLines, mapPair
  , sort, group, nub, transpose, intercalate, isPrefixOf
  , isLower, isAlpha, isDigit, ord, chr
  , inRange
  , toList
  , on
  , (&), (>>>)
  )
where

import Data.List (sort, group, nub, transpose, intercalate, isPrefixOf, findIndex)
import Data.Char (isLower, isAlpha, isDigit, ord, chr)
import Data.Ix (inRange)
import Data.Foldable (toList)

import Data.Function ((&), on)
import Control.Arrow ((>>>))

import Text.Printf (printf)
import Data.Maybe (fromJust)

solution :: (Show a, Show b) => Int -> (String -> (a, b)) -> IO ()
solution day solver = do
    (p1, p2) <- solver <$> readInput day
    print p1; print p2
  where readInput = readFile . printf "../input/%02d.txt"

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn a xs = case break (== a) xs of
    (_, [])      -> [xs]
    (xs, _ : ys) -> xs : splitOn a ys

splitOnFirst :: Eq a => a -> [a] -> ([a], [a])
splitOnFirst a xs = (ys, drop 1 zs)
  where (ys, zs) = break (== a) xs

pierceAt :: Int -> [a] -> ([a], [a])
pierceAt i xs = drop 1 <$> splitAt i xs

elemIx :: Eq a => a -> [a] -> Int
elemIx a = findIx (== a)

findIx :: (a -> Bool) -> [a] -> Int
findIx f = fromJust . findIndex f

mapLines :: (String -> a) -> String -> [a]
mapLines f = map f . lines

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)
