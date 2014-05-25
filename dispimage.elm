import Mouse

choose (w, h) = image 500 500 (if w < 500
                then "/IMG10.JPG"
                else "/DSC_0003.JPG")

main = lift choose Mouse.position
