import Window
import Keyboard
import Time
import Tiles (..)

------------------------------------------------------------------------------------
-- frp specific code
------------------------------------------------------------------------------------

main = lift2 display Window.dimensions gameState

type Input = { x: Int, y: Int, ss: Bool, t: Time}
type TimeInput = (Time, Input)

userInput : Signal TimeInput
userInput = Time.timestamp <| Input <~ lift .x Keyboard.arrows
                                     ~ lift .y Keyboard.arrows
                                     ~ Keyboard.space
                                     ~ Time.every second
defaultGame : GameState
defaultGame = {rows = initialRows 3, score = 0, countdown = 5, currentcount = 5}

newGame : Int -> GameState
newGame rnd = {rows = initialRows rnd, score = 0, countdown = 5, currentcount = 5}

setState : Int -> Int -> Int -> GameState -> GameState
setState x y rv r =
       if | x == 1 -> swipeAndAdd RightToLeft r rv
          | x == -1 -> swipeAndAdd LeftToRight r rv
          | y == 1 -> swipeAndAdd TopToBottom r rv
          | y == -1 -> swipeAndAdd BottomToTop r rv
          | otherwise -> if (r.currentcount <= 0)
                         then swipeAndAdd NoSwipe r rv
                         else {r | currentcount <- r.currentcount - 1}

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

showVal : String -> Int -> Element
showVal label score =
    let
        g = group [rect 100 50 |> filled lightGrey,
                   toForm (flow down [plainText label,
                                      show score |> toText |> rightAligned])]
    in
        collage 100 100 [g]

showScore : Int -> Element
showScore score = showVal "Score" score

showCountdown : Int -> Element
showCountdown count = showVal "Countdown" count

display : (Int, Int) -> GameState -> Element
display (w,h) gs = container w h middle (flow down [ flow right [showCountdown gs.currentcount, spacer 200 50, showScore gs.score],
                                                     if gameOver gs then plainText "Game Over - Hit space bar to try again"
                                                                    else plainText "",
                                                     (grid 400 gs.rows) ])
gameState = foldp stepGame defaultGame userInput
