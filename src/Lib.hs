module Lib
  ( Pos (..), Grid (..)
    , (!), fromList
  , solution, splitOn, splitOnFirst, pierceAt, elemIx, findIx, count, mapLines, pmap
  , sort, group, nub, transpose, intercalate, isPrefixOf, find
    , minimumBy, maximumBy, groupBy, sortOn
  , isLower, isAlpha, isDigit, ord, chr, digitToInt
  , inRange
  , toList
  , comparing
  , first, second, bimap
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
import Data.Bifunctor (first, second, bimap)

import Data.Function ((&))
import Control.Arrow ((>>>))

import Control.Monad.State

import Text.Printf (printf)
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map

data Pos = Pos { x, y :: Int }
  deriving (Eq, Ord)

data Grid a = Grid
    { elems :: Map Pos a
    , height, width :: Int }

instance Num Pos where
    (Pos x y) + (Pos x' y') = Pos (x + x') (y + y')
    (Pos x y) - (Pos x' y') = Pos (x - x') (y - y')
    abs (Pos x y) = Pos (abs x) (abs y)
    (*) = undefined; signum = undefined; fromInteger = undefined

-- every sub-list must be of the same length
fromList :: [[a]] -> Grid a
fromList xxs = concat xxs
    & zipWith (\i x -> (ixToPos i w, x)) [0..]
    & \pxs -> Grid (Map.fromList pxs) h w
  where ixToPos i w = Pos (i `rem` w) (i `div` w)
        (h, w) = (length xxs, length (head xxs))

(!) :: Grid a -> Pos -> a
(!) = (Map.!) . elems

solution :: (Show a, Show b) => Int -> (String -> (a, b)) -> IO ()
solution day solver = do
    (p1, p2) <- solver <$> readInput day
    print p1; print p2
  where readInput = readFile . printf "input/%02d.txt"

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
