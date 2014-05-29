-- build tiles

-- redSquare : Float -> Form
-- redSquare n = traced (solid red) (lines n)

lines : Float -> Form
lines n = traced (solid red) (path [ (n,n), (n,-n), (-n,-n), (-n,n), (n,n) ])

gridLines : Float -> Form
gridLines n =
          let
              l = n / 2
          in
              traced (solid blue) (path [ (-n,-l), (n, -l), (n,0), (-n, 0), (-n,l), (n,l), (n, n), (l,n), (l,-n), (0,-n), (0,n), (-l,n), (-l,-n) ])

sq : Float -> Form
sq n = let clr = rgb 200 200 80
       in filled clr (rect n n)

sqArray : Float -> [Form]
sqArray n = let
                l = n / 2
                l2 = n + l
                s = [(-l2, l2), (-l, l2), (l, l2), (l2, l2),
                     (-l2, l), (-l, l), (l, l), (l2, l),
                     (-l2, -l), (-l, -l), (l, -l), (l2, -l),
                     (-l2, -l2), (-l, -l2), (l, -l2), (l2, -l2)
                    ]
            in
                map (\x -> move x (filled red (square (n - 10)))) s

grid : Float -> Element
grid n =
     let
         l = n / 2
         c = round n
         s = l / 2
     in
         collage c c
         ([ sq n ,
           lines l ,
           gridLines l

          ] ++ (sqArray s))

main = grid 400
