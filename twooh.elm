import Window
import Keyboard
import Time
import Tiles exposing (..)

------------------------------------------------------------------------------------
-- frp specific code
------------------------------------------------------------------------------------

main = lift2 display Window.dimensions gameState

type Input = { x: Int, y: Int, ss: Bool}
type TimeInput = (Time, Input)

userInput : Signal TimeInput
userInput = Time.timestamp <| Input <~ lift .x Keyboard.arrows
                                     ~ lift .y Keyboard.arrows
                                     ~ Keyboard.space

defaultGame : GameState
defaultGame = {rows = initialRows 3, score = 0}

newGame : Int -> GameState
newGame rnd = {rows = initialRows rnd, score = 0}

setState : Int -> Int -> Int -> GameState -> GameState
setState x y rv r =
       if | x == 1 -> swipeAndAdd RightToLeft r rv
          | x == -1 -> swipeAndAdd LeftToRight r rv
          | y == 1 -> swipeAndAdd TopToBottom r rv
          | y == -1 -> swipeAndAdd BottomToTop r rv
          | otherwise -> r

gameOver : GameState -> Bool
gameOver gs =
    numEmpty (flatten gs.rows) == 0 &&
    gs == swipe TopToBottom gs &&
    gs == swipe BottomToTop gs &&
    gs == swipe RightToLeft gs &&
    gs == swipe LeftToRight gs


stepGame : TimeInput -> GameState -> GameState
stepGame (rv, {x, y, ss}) gameState =
    let
        rnd = ((round rv) `mod` 100)
    in
        if ss then newGame rnd else setState x y rnd gameState

showScore : Int -> Element
showScore score =
    let
        g = (group [(filled lightGrey (rect 100 50)), (toForm (flow down [(plainText ("Score")),
                                                                           (rightAligned (toText (show score)))]))])
    in
        collage 100 100 [g]

display : (Int, Int) -> GameState -> Element
display (w,h) gs = container w h middle (flow down [ showScore gs.score,
                                                     if gameOver gs then plainText "Game Over - Hit space bar to try again"
                                                                    else plainText "",
                                                     (grid 400 gs.rows) ])

gameState = foldp stepGame defaultGame userInput
