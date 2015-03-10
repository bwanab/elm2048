module Tiles where
import Window
import Keyboard
import Time
import Dict (fromList, get)
import Maybe ( Maybe(..), withDefault)
import List (map, take, (::), drop, head, length, filter, map2, concat, foldr, tail, reverse, repeat, sort)
import Color (darkGrey, lightGrey, grey, lightYellow, darkYellow, lightOrange, orange, darkOrange, lightRed, red, darkRed, green, charcoal)
import Graphics.Collage (filled, rect, Form, toForm, move, collage, square)
import Graphics.Element(..)
import Text (centered, style, fromString, defaultStyle)

-----------------------------------------------------------------------------------------
-- these functions are generic functionality
-----------------------------------------------------------------------------------------

-- splits a list into groups of n size
splitAll : Int -> List a -> List (List a)
splitAll n x = if | x == [] -> []
                  | otherwise -> (take n x) :: (splitAll 4 (drop n x))

-- transpose a matrix
transpose : List (List a) -> List (List a)
transpose r = map (\n -> map (\row -> head (drop n row)) r ) [0..((length (head r)) - 1)]

-- returns a list of indices to items in list that equal val
elemIndices : a -> List a -> List Int
elemIndices val l = filter (\v -> v >= 0) (map (\(i,v) -> if v == val then i else -1) (map2 (,) [0..(length l)] l))

-- return all the permutations of a list
permutations : List a -> List b -> List (a,b)
permutations xs ys =  concat (map (\y -> (map (\x -> (x, y)) xs)) ys)

-- flattens a list of lists to a single list. Note if the a values are themselves lists they won't be flatten further.
flatten : List (List a) -> List a
flatten rows = foldr (++) [] rows

-- iterate - for a function f on val a with list l invokes f(a,x1) then f(f(a,x2),... for each element xn of l
-- this is different from haskell's iterate since elm isn't lazy.
iterate : (a -> b -> a) -> a -> List b -> List a
iterate f a l = if | l == [] -> []
                   | otherwise -> let
                                      v = (f a (head l))
                                  in
                                      v :: (iterate f v (tail l))
-----------------------------------------------------------------------------------------
-- the non-frp part of the 2048 engine
-----------------------------------------------------------------------------------------


type Tile = Empty | T2 | T4 | T8 | T16 | T32 | T64 | T128 | T256 | T512 | T1024 | T2048
--          deriving (Show, Eq, Ord, Enum)
type alias GameState = {rows : List (List Tile), score : Int, countdown: Int, currentcount: Int}

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
tileStr t = toString (tileNum t)

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

tiles : Tile -> List Tile
tiles t = let
            n = tileSucc t
          in
            if | n == Empty -> [t]
               | otherwise -> t :: (tiles n)



--tileList = ["Empty","T2","T4","T8","T16","T32","T64","T128","T256","T512","T1024","T2048"]
tileList = map tileStr (tiles Empty)

m = fromList (map2 (,) [1..15] [0,1,2,2,4,3,5,7,3,8,1,10,5,9,13])



numEmpty : List Tile -> Int
numEmpty l = length (filter (\x -> x == Empty) l)

replaceEmpty : Int -> Tile -> List Tile -> List Tile
replaceEmpty n t l = (take n l) ++ [t] ++ (drop (n + 1) l)

type Rotation = TLeft | TRight | Flip


rotate : Rotation -> List (List Tile) -> List (List Tile)
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
evalTile : Tile -> List Tile -> List Tile
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

swipeRow : List Tile -> List Tile
swipeRow tiles =
   let
       removeEmpty = (\r -> filter (\t -> t /= Empty) r)
       padEmpty = (\r -> r ++ (take (4 - (length r)) (repeat 4 Empty)))
   in
       removeEmpty tiles |> reverse |> foldr evalTile [] |> removeEmpty |> reverse |> padEmpty

addTiles : Tile -> Int -> Int
addTiles x y = tileNum x + y

sortTiles : List Tile -> List Tile
sortTiles tiles = map tileNum tiles |> sort |> map numTile

computeRow : List Tile -> List Tile -> Int
computeRow new old =
   let
       h = sortTiles old |> reverse |> head
   in
       foldr addTiles 0 (filter (\n -> (tileNum n) > (tileNum h)) new)

type SwipeDirection = RightToLeft | TopToBottom | BottomToTop | LeftToRight | NoSwipe

swipe : SwipeDirection -> GameState -> GameState
swipe dir gs =
        if | dir == BottomToTop -> rotateGame TRight gs |> swipe RightToLeft |> rotateGame TLeft
           | dir == LeftToRight -> rotateGame Flip gs |> swipe RightToLeft |> rotateGame Flip
           | dir == TopToBottom -> rotateGame TLeft gs |> swipe RightToLeft |> rotateGame TRight
           | dir == RightToLeft ->
                 let
                     x = map swipeRow gs.rows
                 in
                     let
                        earned = map2 (,) x gs.rows |> map (\(n,o) -> computeRow n o) |> foldr (+) 0
                     in
                        {gs | rows <- x,
                              score <- gs.score + earned }

swipeAndAdd : SwipeDirection -> GameState -> Int -> GameState
swipeAndAdd dir gs rv =
    let
       r = if dir == NoSwipe then gs else swipe dir gs
    in
       if ((r == gs) && (dir /= NoSwipe)) || (numEmpty (flatten r.rows) == 0)
          then {gs | currentcount <- gs.countdown + 1}
          else let
                  l = flatten r.rows
                  re = replaceOneEmpty rv l
               in
                  {r | rows <- splitAll 4 re, currentcount <- r.countdown + 1}

getEmptyIndex : Int -> List Tile -> Int
getEmptyIndex n l = elemIndices Empty l |> drop n |> head

replaceOneEmpty : Int -> List Tile -> List Tile
replaceOneEmpty rv l =
    let
       t = if rv % 4 == 0 then T4 else T2
    in
       replaceEmpty (getEmptyIndex (rv % (numEmpty l)) l) t l

ir : List Tile
ir = repeat 16 Empty

initialRows : Int -> List (List Tile)
--initialRows rnd = replaceOneEmpty rnd ir |> replaceOneEmpty (round (toFloat rnd / 10)) |> splitAll 4
initialRows rnd = replaceOneEmpty rnd ir |> splitAll 4

tc = [darkGrey, lightGrey, grey,
      lightYellow, darkYellow, lightOrange,
      orange, darkOrange, lightRed,
      red, darkRed, green]


tileColor = fromList (map2 (,) tileList tc)

sq : Float -> Form
sq n = let clr = charcoal
       in filled clr (rect n n)


locations : Float -> List (Float, Float)
locations n = let
                l = n / 2
                l2 = n + l
                s = [-l2, -l, l, l2]
            in
                permutations (reverse s) s

numberStyle = {defaultStyle | bold <- True,
                              height <- Just 36}

sqArray : List ((Float,Float),Tile) -> Float -> List Form
sqArray s size =
        let
           color x = (withDefault darkGrey (get (tileStr (snd x)) tileColor))
           t x = let
                    n = (tileNum (snd x))
                 in
                    toForm (centered (style numberStyle (fromString (if n > 0 then (toString n) else ""))))
           len = round size
        in
           map (\x -> move (fst x) (toForm (collage len len [(filled (color x) (square size)), (t x)]))) s

grid : Float -> List (List Tile) -> Element
grid n rows =
     let
         l = n / 2
         c = round n
         s = l / 2
         locs = locations s
     in
         collage c c
         ([ sq n ] ++ (sqArray (map2 (,) locs (flatten rows)) (s - 10)))

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
