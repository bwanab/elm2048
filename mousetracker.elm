import Mouse

dim (w, h) = asText (if w < 500 then "yes" else "no")
main = lift dim Mouse.position
