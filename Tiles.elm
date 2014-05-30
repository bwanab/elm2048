module Tiles where

-- data Tile = Empty | T2 | T4 | T8 | T16 | T32 | T64 | T128 | T256 | T512 | T1024 | T2048
--          deriving (Show, Eq, Ord, Enum)

data Tile = T Int
empty = T 0
t2 = T 2
t4 = T 4
t8 = T 8
t16 = T 16
t32 = T 32
t64 = T 64
t128 = T 128
t256 = T 256
t512 = T 512
t1024 = T 1024
t2048 = T 2048

splitAll : Int -> [a] -> [[a]]
splitAll n x = if | x == [] -> []
                  | otherwise -> (take n x) :: (splitAll 4 (drop n x))

-- splitAll : Int -> [a] -> [[a]]
-- splitAll n x = if | x == [] -> []
--                   | otherwise ->
--                      let
--                         (y,ys) = splitAt n x
--                      in
--                         y::splitAll n ys
