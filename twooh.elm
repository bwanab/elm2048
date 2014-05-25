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

grid : Float -> Element
grid n =
     let
         l = n / 2
         c = round n
     in
         collage c c
         [ sq n ,
           lines l ,
           gridLines l
         ]

main = grid 800
