import Tiles (swipe2)
import List (map)

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


t : List (List Tile) -> List (List Tile)
t x = map swipe2 x

main = t r

-- d = flatten (repeat 20 [TopToBottom, BottomToTop, RightToLeft, LeftToRight])

-- bad = [[T4,T2,T8,Empty],[T4,T16,Empty,Empty],[T4,T2,Empty,Empty],[T2,Empty,Empty,Empty]]

-- badDir = LeftToRight
