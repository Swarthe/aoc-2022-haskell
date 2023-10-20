module Lib
  ( readInput, splitOn, splitOnFirst, pierceAt, elemIx, findIx, mapLines, mapPair
  , sort, group, nub, transpose, isPrefixOf
  , fromJust
  , isLower, isAlpha, isDigit, ord, chr
  , inRange
  , (&), (>>>)
  )
where

import Data.List (sort, group, nub, transpose, isPrefixOf, findIndex)
import Data.Maybe (fromJust)
import Data.Char (isLower, isAlpha, isDigit, ord, chr)
import Data.Ix (inRange)

import Data.Function ((&))
import Control.Arrow ((>>>))

import Text.Printf (printf)

readInput :: Int -> IO String
readInput = readFile . printf "../input/%02d.txt"

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn a xs = case break (== a) xs of
    (_, [])      -> [xs]
    (xs, _ : ys) -> xs : splitOn a ys

splitOnFirst :: Eq a => a -> [a] -> ([a], [a])
splitOnFirst a xs = (ys, drop 1 zs)
  where (ys, zs) = break (== a) xs

deleteAt :: Int -> [a] -> [a]
deleteAt i l = ys ++ xs
  where (xs, ys) = pierceAt i l

pierceAt :: Int -> [a] -> ([a], [a])
pierceAt i l = drop 1 <$> splitAt i l

elemIx :: Eq a => a -> [a] -> Int
elemIx a = findIx (== a)

findIx :: (a -> Bool) -> [a] -> Int
findIx f = fromJust . findIndex f

mapLines :: (String -> a) -> String -> [a]
mapLines f = map f . lines

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)
