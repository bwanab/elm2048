-- Module Tile
-- ( swipe
-- , rotate
-- ) where

import Data.List

data Tile = Empty | T2 | T4 | T8 | T16 | T32 | T64 | T128 | T256 | T512 | T1024 | T2048
          deriving (Show, Eq, Ord, Enum)

-- join :: Tile -> Tile -> [Tile]
-- join x y | x == y = [succ x]
--          | otherwise = [x, y]

-- swipe :: [Tile] -> [Tile]
-- swipe [] = []
-- swipe (x:xs) | x == Empty = swipe xs ++ [Empty]
--              | otherwise = x: (swipe xs)

swipe1 :: [Tile] -> [Tile]
swipe1 [] = []
swipe1 (x:xs) | x == Empty = swipe1 xs ++ [Empty]
              | xs == [] = [x]
              | otherwise = let
                                y = head xs
                            in
                                if x == y then (succ x):swipe1 (tail xs)
                                          else x: (swipe1 xs)

swipeRow :: [Tile] -> [Tile]
swipeRow row = let l = iterate swipe1 row
                   l1 = group l
                   l2 = dropWhile (\x -> length x == 1) (map (\x -> take 6 x) l1)
                   l3 = head (head l2)
               in
                   l3 ++ take (4 - (length l3)) (repeat Empty)

data SwipeDirection = RightToLeft | TopToBottom | BottomToTop | LeftToRight

swipe :: SwipeDirection -> [[Tile]] -> [[Tile]]
swipe RightToLeft rows = map (\row -> swipeRow row) rows
swipe BottomToTop rows = rotate TLeft (swipe RightToLeft (rotate TLeft rows))
swipe LeftToRight rows = rotate Flip (swipe RightToLeft (rotate Flip rows))
swipe TopToBottom rows = rotate TRight (swipe RightToLeft (rotate TRight rows))

r = [[Empty, T2, Empty, T2],
     [T4   , Empty, T2, T2],
     [T2, Empty, Empty, T2],
     [Empty, T2, T4, T4]]


data Rotation = TLeft | TRight | Flip

rotate :: Rotation -> [[Tile]] -> [[Tile]]
rotate TLeft rows = reverse rows
rotate TRight rows = map (\l -> reverse l) rows
rotate Flip rows = rotate TLeft (rotate TRight rows)
