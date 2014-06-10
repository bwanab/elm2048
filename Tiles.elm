module Tiles where
import Window
import Keyboard
import Time
import Dict

-----------------------------------------------------------------------------------------
-- these functions are generic functionality
-----------------------------------------------------------------------------------------

-- splits a list into groups of n size
splitAll : Int -> [a] -> [[a]]
splitAll n x = if | x == [] -> []
                  | otherwise -> (take n x) :: (splitAll 4 (drop n x))

-- transpose a matrix
transpose : [[a]] -> [[a]]
transpose r = map (\n -> map (\row -> head (drop n row)) r ) [0..3]

-- returns a list of indices to items in list that equal val
elemIndices : a -> [a] -> [Int]
elemIndices val l = filter (\v -> v >= 0) (map (\(i,v) -> if v == val then i else -1) (zip [0..(length l)] l))

-- return all the permutations of a list
permutations : [a] -> [b] -> [(a,b)]
permutations xs ys =  concat (map (\y -> (map (\x -> (x, y)) xs)) ys)

-- flattens a list of lists to a single list. Note if the a values are themselves lists they won't be flatten further.
flatten : [[a]] -> [a]
flatten rows = foldr (++) [] rows

-- iterate - for a function f on val a with list l invokes f(a,x1) then f(f(a,x2),... for each element xn of l
-- this is different from haskell's iterate since elm isn't lazy.
iterate : (a -> b -> a) -> a -> [b] -> [a]
iterate f a l = if | l == [] -> []
                   | otherwise -> let
                                      v = (f a (head l))
                                  in
                                      v :: (iterate f v (tail l))
-----------------------------------------------------------------------------------------
-- the non-frp part of the 2048 engine
-----------------------------------------------------------------------------------------


data Tile = Empty | T2 | T4 | T8 | T16 | T32 | T64 | T128 | T256 | T512 | T1024 | T2048
--          deriving (Show, Eq, Ord, Enum)
type GameState = {rows : [[Tile]], score : Int}

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

numTile : Int -> Tile
numTile t = case t of
                 0 -> Empty
                 2 -> T2
                 4 -> T4
                 8 -> T8
                 16 -> T16
                 32 -> T32
                 64 -> T64
                 128 -> T128
                 256 -> T256
                 512 -> T512
                 1024 -> T1024
                 2048 -> T2048

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

m = Dict.fromList (zip [1..15] [0,1,2,2,4,3,5,7,3,8,1,10,5,9,13])



numEmpty : [Tile] -> Int
numEmpty l = length (filter (\x -> x == Empty) l)

replaceEmpty : Int -> Tile -> [Tile] -> [Tile]
replaceEmpty n t l = (take n l) ++ [t] ++ (drop (n + 1) l)

data Rotation = TLeft | TRight | Flip


rotate : Rotation -> [[Tile]] -> [[Tile]]
rotate r rows = if | r == TLeft ->  map (\row -> reverse row) (transpose rows)
                   | r == TRight -> reverse (transpose rows)
                   | r == Flip -> map (\row -> reverse row) rows

rotateGame : Rotation -> GameState -> GameState
rotateGame r gs = {gs | rows <- (rotate r gs.rows) }

-- look at an element x and row so far.
-- If row is empty then just put x on result
-- else, look at last entered element, if it equals y, then we have a match, so consolidate and pad with empty
--                                     if it is empty, them discard both it and x
--                                     otherwise, add x to the existing row.
evalTile : Tile -> [Tile] -> [Tile]
evalTile x row =
   if | row == [] -> [x]
      | x == Empty -> tail row
      | otherwise ->
          let
             y = head row
             ys = tail row
          in
             if | x == y -> tileSucc x :: Empty :: ys
                | y == Empty -> ys
                | otherwise -> x::row

swipeRow : [Tile] -> [Tile]
swipeRow tiles =
   let
       removeEmpty = (\r -> filter (\t -> t /= Empty) r)
       padEmpty = (\r -> r ++ (take (4 - (length r)) (repeat 4 Empty)))
   in
       removeEmpty tiles |> reverse |> foldr evalTile [] |> removeEmpty |> reverse |> padEmpty

addTiles : Tile -> Int -> Int
addTiles x y = tileNum x + y

sortTiles : [Tile] -> [Tile]
sortTiles tiles = map tileNum tiles |> sort |> map numTile

computeRow : [Tile] -> [Tile] -> Int
computeRow new old =
   let
       h = sortTiles old |> reverse |> head
   in
       foldr addTiles 0 (filter (\n -> (tileNum n) > (tileNum h)) new)

data SwipeDirection = RightToLeft | TopToBottom | BottomToTop | LeftToRight

swipe : SwipeDirection -> GameState -> GameState
swipe dir gs =
        if | dir == BottomToTop -> rotateGame TLeft (swipe RightToLeft (rotateGame TRight gs))
           | dir == LeftToRight -> rotateGame Flip (swipe RightToLeft (rotateGame Flip gs))
           | dir == TopToBottom -> rotateGame TRight (swipe RightToLeft (rotateGame TLeft gs))
           | dir == RightToLeft ->
                 let
                     x = map swipeRow gs.rows
                 in
                     let
                        earned = zip x gs.rows |> map (\(n,o) -> computeRow n o) |> foldr (+) 0
                     in
                        {gs | rows <- x,
                              score <- gs.score + earned }

swipeAndAdd : SwipeDirection -> GameState -> Int -> GameState
swipeAndAdd dir gs rv =
    let
       r = swipe dir gs
    in
       if r == gs
          then gs
          else let
                  l = flatten r.rows
                  re = replaceOneEmpty rv l
               in
                  {r | rows <- (splitAll 4 re) }

getEmptyIndex : Int -> [Tile] -> Int
getEmptyIndex n l = head (drop n (elemIndices Empty l))

replaceOneEmpty : Int -> [Tile] -> [Tile]
replaceOneEmpty rv l =
    let
       t = if rv `mod` 4 == 0 then T4 else T2
    in
       replaceEmpty (getEmptyIndex (rv `mod` (numEmpty l)) l) t l

ir : [Tile]
ir = repeat 16 Empty

initialRows : [[Tile]]
initialRows = replaceOneEmpty 1 ir |> replaceOneEmpty 1 |> splitAll 4

tc = [darkGrey, lightGrey, grey,
      lightYellow, darkYellow, lightOrange,
      orange, darkOrange, lightRed,
      red, darkRed, green]


tileColor = Dict.fromList (zip tileList tc)

sq : Float -> Form
sq n = let clr = charcoal
       in filled clr (rect n n)


locations : Float -> [(Float, Float)]
locations n = let
                l = n / 2
                l2 = n + l
                s = [-l2, -l, l, l2]
            in
                permutations (reverse s) s


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

------------------------------------------------------------------------------------
-- test code follows
------------------------------------------------------------------------------------

r = [
     [Empty, Empty, Empty, Empty],
     [Empty, Empty, Empty, T2],
     [Empty, Empty, T2, Empty],
     [Empty, Empty, T2, T2],
     [Empty, T2, Empty, Empty],
     [Empty, T2, Empty, T2],
     [Empty, T2, T2, Empty],
     [Empty, T2, T2, T2],
     [T2, Empty, Empty, Empty],
     [T2, Empty, Empty, T2],
     [T2, Empty, T2, Empty],
     [T2, Empty, T2, T2],
     [T2, T2, Empty, Empty],
     [T2, T2, Empty, T2],
     [T2, T2, T2, Empty],
     [T2, T2, T2, T2]]
