import Tiles

sq : Float -> Form
sq n = let clr = rgb 200 200 80
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

sqArray : [(Float,Float)] -> Float -> [Form]
sqArray s size = map (\x -> move x (filled red (square size))) s

grid : Float -> Element
grid n =
     let
         l = n / 2
         c = round n
         s = l / 2
         locs = locations s
     in
         collage c c
         ([ sq n ] ++ (sqArray locs (s - 10)))

main = grid 400
