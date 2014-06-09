import Window
import Keyboard
import Tiles (..)

------------------------------------------------------------------------------------
-- frp specific code
------------------------------------------------------------------------------------

main = lift2 display Window.dimensions gameState

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
