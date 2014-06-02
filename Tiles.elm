module Tiles where

import Dict

data Tile = Empty | T2 | T4 | T8 | T16 | T32 | T64 | T128 | T256 | T512 | T1024 | T2048
--          deriving (Show, Eq, Ord, Enum)

tileNum : Tile -> Int
tileNum t = case t of
                 Empty -> 0
                 T2 -> 2
                 T4 -> 4
                 T8 -> 8
                 T16 -> 16
                 T32 -> 32
                 T64 -> 64
                 T128 -> 128
                 T256 -> 256
                 T512 -> 512
                 T1024 -> 1024
                 T2048 -> 2048

tileStr : Tile -> String
tileStr t = show (tileNum t)

tileSucc : Tile -> Tile
tileSucc t = case t of
                 Empty -> T2
                 T2 -> T4
                 T4 -> T8
                 T8 -> T16
                 T16 -> T32
                 T32 -> T64
                 T64 -> T128
                 T128 -> T256
                 T256 -> T512
                 T512 -> T1024
                 T1024 -> T2048
                 T2048 -> Empty

tiles : Tile -> [Tile]
tiles t = let
            n = (tileSucc t)
          in
            if | n == Empty -> [t]
               | otherwise -> t :: (tiles n)



--tileList = ["Empty","T2","T4","T8","T16","T32","T64","T128","T256","T512","T1024","T2048"]
tileList = map tileStr (tiles Empty)

m = Dict.fromList (zip [1..15] [1,1,2,2,4,3,5,7,3,8,1,10,5,9,13])

splitAll : Int -> [a] -> [[a]]
splitAll n x = if | x == [] -> []
                  | otherwise -> (take n x) :: (splitAll 4 (drop n x))


numEmpty : [Tile] -> Int
numEmpty l = length (filter (\x -> x == Empty) l)

replaceEmpty : Int -> Tile -> [Tile] -> [Tile]
replaceEmpty n t l = (take n l) ++ [t] ++ (drop (n + 1) l)

data Rotation = TLeft | TRight | Flip

transpose : [[a]] -> [[a]]
transpose r = map (\n -> map (\row -> head (drop n row)) r ) [0..3]

rotate : Rotation -> [[Tile]] -> [[Tile]]
rotate r rows = if | r == TLeft ->  map (\row -> reverse row) (transpose rows)
                   | r == TRight -> reverse (transpose rows)
                   | r == Flip -> map (\row -> reverse row) rows

swipe1 : [Tile] -> [Tile]
swipe1 row = if | row == [] -> []
                | otherwise -> let
                                x = head row
                                xs = tail row
                               in
                                if | x == Empty -> swipe1 xs ++ [Empty]
                                   | xs == [] -> [x]
                                   | otherwise -> let
                                                   y = head xs
                                                  in
                                                   if x == y then (tileSucc x) :: swipe1 (tail xs)
                                                             else x :: (swipe1 xs)

swipeRow : [Tile] -> [Tile]
swipeRow row = swipe1 (swipe1 row)

data SwipeDirection = RightToLeft | TopToBottom | BottomToTop | LeftToRight

swipe : SwipeDirection -> [[Tile]] -> [[Tile]]
swipe dir rows = if | dir == RightToLeft -> map (\row -> swipeRow row) rows
                    | dir == BottomToTop -> rotate TLeft (swipe RightToLeft (rotate TRight rows))
                    | dir == LeftToRight -> rotate Flip (swipe RightToLeft (rotate Flip rows))
                    | dir == TopToBottom -> rotate TRight (swipe RightToLeft (rotate TLeft rows))


swipeAndAdd : SwipeDirection -> [[Tile]] -> [[Tile]]
swipeAndAdd dir rows = let
                          r = swipe dir rows
                          l = flatten r
                          re = replaceOneEmpty l
                       in
                          splitAll 4 re


elemIndices : a -> [a] -> [Int]
elemIndices val l = filter (\v -> v > 0) (map (\(i,v) -> if v == val then i else -1) (zip [0..(length l)] l))

getEmptyIndex : Int -> [Tile] -> Int
getEmptyIndex n l = head (drop n (elemIndices Empty l))

replaceOneEmpty : [Tile] -> [Tile]
replaceOneEmpty l = replaceEmpty (getEmptyIndex (maybe 1 (\n -> n) (Dict.get (numEmpty l) m)) l) T2 l

ir : [Tile]
ir = repeat 16 Empty

initialRows : [[Tile]]
initialRows = splitAll 4 (replaceOneEmpty (replaceOneEmpty ir))

tc = [darkGrey, lightGrey, grey,
      lightYellow, darkYellow, lightOrange,
      orange, darkOrange, lightRed,
      red, darkRed, green]


tileColor = Dict.fromList (zip tileList tc)

sq : Float -> Form
sq n = let clr = charcoal
       in filled clr (rect n n)

permutations : [a] -> [b] -> [(a,b)]
permutations xs ys =  concat (map (\y -> (map (\x -> (x, y)) xs)) ys)

locations : Float -> [(Float, Float)]
locations n = let
                l = n / 2
                l2 = n + l
                s = [-l2, -l, l, l2]
            in
                permutations (reverse s) s

flatten : [[Tile]] -> [Tile]
flatten rows = foldr (++) [] rows

sqArray : [((Float,Float),Tile)] -> Float -> [Form]
sqArray s size =
        let
           color x = (Dict.getOrElse darkGrey (tileStr (snd x)) tileColor)
           t x = let
                    n = (tileNum (snd x))
                 in
                    toForm (plainText (if n > 0 then (show n) else ""))
           len = round size
        in
           map (\x -> move (fst x) (toForm (collage len len [(filled (color x) (square size)), (t x)]))) s

grid : Float -> [[Tile]] -> Element
grid n rows =
     let
         l = n / 2
         c = round n
         s = l / 2
         locs = locations s
     in
         collage c c
         ([ sq n ] ++ (sqArray (zip locs (flatten rows)) (s - 10)))
