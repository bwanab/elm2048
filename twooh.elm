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
                                     ~ Time.every (2 * second)
defaultGame : GameState
defaultGame = {rows = initialRows 3, score = 0, t = 0}

newGame : Int -> GameState
newGame rnd = {rows = initialRows rnd, score = 0, t = 0}

setState : Int -> Int -> Int -> Time -> GameState -> GameState
setState x y rv t r =
       if | x == 1 -> swipeAndAdd RightToLeft r rv
          | x == -1 -> swipeAndAdd LeftToRight r rv
          | y == 1 -> swipeAndAdd TopToBottom r rv
          | y == -1 -> swipeAndAdd BottomToTop r rv
          | otherwise -> if (r.t /= t)
                         then
                            let
                                ns = swipeAndAdd NoSwipe r rv
                            in
                                {ns | t <- t}
                         else r

gameOver : GameState -> Bool
gameOver gs =
    numEmpty (flatten gs.rows) == 0 &&
    gs == swipe TopToBottom gs &&
    gs == swipe BottomToTop gs &&
    gs == swipe RightToLeft gs &&
    gs == swipe LeftToRight gs


stepGame : TimeInput -> GameState -> GameState
stepGame (rv, {x, y, ss, t}) gameState =
    let
        rnd = ((round rv) `mod` 100)
    in
        if ss then newGame rnd else setState x y rnd t gameState

showScore : Int -> Element
showScore score =
    let
        g = group [rect 100 50 |> filled lightGrey,
                   toForm (flow down [plainText "Score",
                                      show score |> toText |> rightAligned])]
    in
        collage 100 100 [g]

display : (Int, Int) -> GameState -> Element
display (w,h) gs = container w h middle (flow down [ flow right [spacer 300 50, showScore gs.score],
                                                     if gameOver gs then plainText "Game Over - Hit space bar to try again"
                                                                    else plainText "",
                                                     (grid 400 gs.rows) ])
gameState = foldp stepGame defaultGame userInput
