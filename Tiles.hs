module Tiles
-- ( swipe
-- , rotate
-- , Tile
-- )
where

import Data.List
import Data.Char
import qualified Data.Map as Map
import Data.Maybe
import System.Random
import System.IO.Unsafe
import System.Environment



data Tile = Empty | T2 | T4 | T8 | T16 | T32 | T64 | T128 | T256 | T512 | T1024 | T2048
          deriving (Show, Eq, Ord, Enum)

swipe1 :: [Tile] -> [Tile]
swipe1 [] = []
swipe1 (x:xs) | x == Empty = swipe1 xs ++ [Empty]
              | xs == [] = [x]
              | otherwise = let
                                y = head xs
                            in
                                if x == y then (succ x):swipe1 (tail xs)
                                          else x: (swipe1 xs)

-- swipeRow iterates swipe1 infinitely, then groups which means the first group with multiple
-- values is the final value.
-- TODO: since elm isn't lazy, this will have to be rethought.
swipeRow :: [Tile] -> [Tile]
swipeRow row = let l = iterate swipe1 row
                   l1 = group l
                   l2 = dropWhile (\x -> length x == 1) (map (\x -> take 6 x) l1)
                   l3 = head (head l2)
               in
                   l3 ++ take (4 - (length l3)) (repeat Empty)

-- to do swiping in whatever direction, we rotate to get that direction as a RightToLeft swipe, do the swipe
-- then rotate it back to the original direction.
data SwipeDirection = RightToLeft | TopToBottom | BottomToTop | LeftToRight

swipe :: SwipeDirection -> [[Tile]] -> [[Tile]]
swipe RightToLeft rows = map (\row -> swipeRow row) rows
swipe BottomToTop rows = rotate TLeft (swipe RightToLeft (rotate TRight rows))
swipe LeftToRight rows = rotate Flip (swipe RightToLeft (rotate Flip rows))
swipe TopToBottom rows = rotate TRight (swipe RightToLeft (rotate TLeft rows))


data Rotation = TLeft | TRight | Flip

rotate :: Rotation -> [[Tile]] -> [[Tile]]
rotate TLeft rows = map (\row -> reverse row) (transpose rows)
rotate TRight rows = reverse (transpose rows)
rotate Flip rows = map (\row -> reverse row) rows

getRnd :: Int -> Int
-- getRnd _ = 1
getRnd m = unsafePerformIO (getStdRandom (randomR (0 , m)))

m = Map.fromList (zip [1..16] (map getRnd [1..16]))

swipeAndAdd :: SwipeDirection -> [[Tile]] -> [[Tile]]
swipeAndAdd dir rows = let
                          r = swipe dir rows
                          l = intercalate [] r
                          i = getEmptyIndex (fromJust (Map.lookup (numEmpty l) m)) l
                          re = replaceEmpty i T2 l
                       in
                          splitAll 4 re

numEmpty :: [Tile] -> Int
numEmpty l = length (filter (== Empty) l)

getEmptyIndex :: Int -> [Tile] -> Int
getEmptyIndex n l = head (drop n (elemIndices Empty l))

replaceEmpty :: Int -> Tile -> [Tile] -> [Tile]
replaceEmpty n t l = (take n l) ++ [t] ++ (drop (n + 1) l)

splitAll :: Int -> [Tile] -> [[Tile]]
splitAll _ [] = []
splitAll n x = let
                  (y,ys) = splitAt n x
               in
                  y:splitAll n ys

ithRow :: Int -> [[Tile]] -> [Tile]
ithRow n rows = head (drop n rows)

printRows :: [[Tile]] -> IO()
printRows rows = do
                        print (ithRow 0 rows)
                        print (ithRow 1 rows)
                        print (ithRow 2 rows)
                        print (ithRow 3 rows)

replaceOneEmpty :: [Tile] -> [Tile]
replaceOneEmpty l = replaceEmpty (getEmptyIndex (fromJust (Map.lookup (numEmpty l) m)) l) T2 l

initialRows :: [[Tile]]
initialRows = let
                l = take 16 (repeat Empty)
                ll = iterate replaceOneEmpty l
                l3 = head (drop 2 ll)
              in
                splitAll 4 l3

doturn :: String -> [[Tile]] -> [[Tile]]
doturn m r | m == "W" = swipeAndAdd BottomToTop r
           | m == "S" = swipeAndAdd TopToBottom r
           | m == "A" = swipeAndAdd RightToLeft r
           | m == "D" = swipeAndAdd LeftToRight r
           | otherwise = r

domain r = do
       printRows r
       putStr "> "
       m <- getLine
       domain (doturn (map toUpper m) r)

main = do
     domain initialRows
