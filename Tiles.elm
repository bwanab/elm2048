module Tiles where
import Window
import Keyboard

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


computeScore : Tile -> Int
computeScore x = 2 * (tileNum x)


swipe1 : ([Tile],[Int]) -> ([Tile],[Int])
swipe1 (row, score) =
             if | row == [] -> ([],[])
                | otherwise -> let
                                x = head row
                                xs = tail row
                               in
                                if | x == Empty -> let
                                                      (r, s) = swipe1 (xs, score)
                                                   in
                                                      ((r ++ [Empty]), s)
                                   | xs == [] -> ([x], score)
                                   | otherwise -> let
                                                   y = head xs
                                                  in
                                                   if x == y then
                                                               let
                                                                  (r, s) = swipe1 (((tail xs) ++ [Empty]), score)
                                                               in
                                                                  (((tileSucc x) :: r),  s ++ [computeScore x])
                                                             else
                                                                let (r, s) = (swipe1 (xs, score))
                                                                in  ((x :: r), s)

data SwipeDirection = RightToLeft | TopToBottom | BottomToTop | LeftToRight

swipe : SwipeDirection -> GameState -> GameState
swipe dir gs =
        if | dir == BottomToTop -> rotateGame TLeft (swipe RightToLeft (rotateGame TRight gs))
           | dir == LeftToRight -> rotateGame Flip (swipe RightToLeft (rotateGame Flip gs))
           | dir == TopToBottom -> rotateGame TRight (swipe RightToLeft (rotateGame TLeft gs))
           | dir == RightToLeft ->
                 let
                     x = map (\row -> swipe1 row) (zip gs.rows (repeat 4 []))
                 in -- we get back a list of (row, [score])
                     {gs | rows <- (map fst x),
                           score <- gs.score + (foldr (+) 0 (map (\(_, l) -> (foldr (+) 0 l)) x)) }

swipeAndAdd : SwipeDirection -> GameState -> GameState
swipeAndAdd dir gs = let
                          r = swipe dir gs
                          l = flatten r.rows
                          re = replaceOneEmpty l
                       in
                          {r | rows <- (splitAll 4 re) }

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
-- frp specific code
------------------------------------------------------------------------------------

type Input = { x: Int, y: Int }
userInput = dropRepeats Keyboard.arrows

defaultGame : GameState
defaultGame = {rows = initialRows, score = 0}

setState : Int -> Int -> GameState -> GameState
setState x y r = if | x == 1 -> swipeAndAdd RightToLeft r
                    | x == -1 -> swipeAndAdd LeftToRight r
                    | y == 1 -> swipeAndAdd TopToBottom r
                    | y == -1 -> swipeAndAdd BottomToTop r
                    | otherwise -> r

stepGame : Input -> GameState -> GameState
stepGame {x, y} gameState = setState x y gameState

showScore : Int -> Element
showScore score =
    let
        g = (group [(filled lightGrey (rect 100 50)), (toForm (flow down [(plainText ("Score")),
                                                                           (rightAligned (toText (show score)))]))])
    in
        collage 100 100 [g]

display : (Int, Int) -> GameState -> Element
display (w,h) {rows, score} = container w h middle (flow down [ showScore score,
                                                                (grid 400 rows) ])

gameState = foldp stepGame defaultGame userInput

main = lift2 display Window.dimensions gameState

------------------------------------------------------------------------------------
-- test code follows
------------------------------------------------------------------------------------

-- r = [
--      [Empty, Empty, Empty, Empty],
--      [Empty, Empty, Empty, T2],
--      [Empty, Empty, T2, Empty],
--      [Empty, Empty, T2, T2],
--      [Empty, T2, Empty, Empty],
--      [Empty, T2, Empty, T2],
--      [Empty, T2, T2, Empty],
--      [Empty, T2, T2, T2],
--      [T2, Empty, Empty, Empty],
--      [T2, Empty, Empty, T2],
--      [T2, Empty, T2, Empty],
--      [T2, Empty, T2, T2],
--      [T2, T2, Empty, Empty],
--      [T2, T2, Empty, T2],
--      [T2, T2, T2, Empty],
--      [T2, T2, T2, T2]]

-- d = flatten (repeat 20 [TopToBottom, BottomToTop, RightToLeft, LeftToRight])

bad = [[T4,T2,T8,Empty],[T4,T16,Empty,Empty],[T4,T2,Empty,Empty],[T2,Empty,Empty,Empty]]

-- badDir = LeftToRight
