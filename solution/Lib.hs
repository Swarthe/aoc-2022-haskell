module Lib
  ( Pos (..)
  , solution, splitOn, splitOnFirst, pierceAt, elemIx, findIx, count, mapLines, pmap
  , sort, group, nub, transpose, intercalate, isPrefixOf, find
    , minimumBy, maximumBy, groupBy, sortOn
  , isLower, isAlpha, isDigit, ord, chr, digitToInt
  , inRange
  , toList
  , comparing
  , (&), (>>>)
  , module Control.Monad.State
  )
where

import Data.List
    ( sort, group, nub, transpose, intercalate, isPrefixOf, find
    , minimumBy, maximumBy, groupBy, sortOn
    , findIndex )

import Data.Char (isLower, isAlpha, isDigit, ord, chr, digitToInt)
import Data.Ix (inRange)
import Data.Foldable (toList)
import Data.Ord (comparing)

import Data.Function ((&))
import Control.Arrow ((>>>))

import Control.Monad.State

import Text.Printf (printf)
import Data.Maybe (fromJust)

data Pos = Pos { x, y :: Int }
  deriving (Eq, Ord)

instance Num Pos where
    (Pos x y) + (Pos x' y') = Pos (x + x') (y + y')
    (Pos x y) - (Pos x' y') = Pos (x - x') (y - y')
    abs (Pos x y) = Pos (abs x) (abs y)
    (*) = undefined; signum = undefined; fromInteger = undefined

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

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

mapLines :: (String -> a) -> String -> [a]
mapLines f = map f . lines

pmap :: (a -> b) -> (a, a) -> (b, b)
pmap f (x, y) = (f x, f y)
