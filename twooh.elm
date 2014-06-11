import Window
import Keyboard
import Time
import Tiles (..)

------------------------------------------------------------------------------------
-- frp specific code
------------------------------------------------------------------------------------

main = lift2 display Window.dimensions gameState

type Input = { x: Int, y: Int}
type TimeInput = (Time, Input)

userInput : Signal TimeInput
userInput = Time.timestamp (dropRepeats Keyboard.arrows)

defaultGame : GameState
defaultGame = {rows = initialRows, score = 0}

setState : Int -> Int -> Int -> GameState -> GameState
setState x y rv r =
       if | x == 1 -> swipeAndAdd RightToLeft r rv
          | x == -1 -> swipeAndAdd LeftToRight r rv
          | y == 1 -> swipeAndAdd TopToBottom r rv
          | y == -1 -> swipeAndAdd BottomToTop r rv
          | otherwise -> r

stepGame : TimeInput -> GameState -> GameState
stepGame (rv, {x, y}) gameState = setState x y ((round rv) `mod` 100) gameState

showScore : Int -> Element
showScore score =
    let
        g = group [rect 100 50 |> filled lightGrey,
                   toForm (flow down [plainText "Score",
                                      show score |> toText |> rightAligned])]
    in
        collage 100 100 [g]

display : (Int, Int) -> GameState -> Element
display (w,h) {rows, score} = container w h middle (flow down [ flow right [spacer 300 50, showScore score],
                                                                grid 400 rows ])

gameState = foldp stepGame defaultGame userInput
