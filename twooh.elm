module Twooh where

import Window
import Keyboard
import Time
import Graphics.Input.Field as Field
import Graphics.Input exposing (dropDown)
import Graphics.Collage exposing (group,rect,filled,toForm,collage)
import Graphics.Element exposing (..)
import Tiles exposing (..)
import Signal exposing (Signal(..),map, map2,map4,merge,foldp )
import Color exposing (lightGrey,red)
import Text exposing (fromString, defaultStyle, style)

------------------------------------------------------------------------------------
-- frp specific code
------------------------------------------------------------------------------------

main = map2 display Window.dimensions gameState

-- difficulty : Field.Content Int
-- difficulty = Field.field 20000

-- difficultyDropdown : Element
-- difficultyDropdown =
--    dropDown difficulty.handle
--       [ ("Trivial", 20000),
--         ("Easy", 40),
--         ("Medium", 15),
--         ("Hard", 5),
--         ("Really Hard", 2),
--         ("Impossible", 1) ]

--type alias Input = { x: Int, y: Int, ss: Bool, t: Time.Time, diff: Int}
type alias Input = { x: Int, y: Int, ss: Bool, t: Time.Time}
type alias TimeInput = (Time.Time, Input)

--port reset : Signal Bool

userInput = Time.timestamp <| map4 Input (Signal.map .x Keyboard.arrows)
                                         (Signal.map .y Keyboard.arrows)
                                         Keyboard.space
                                         (Time.every Time.second)
                                     -- ~ difficulty.signal

defaultGame : GameState
defaultGame = {rows = initialRows 3, score = 0, countdown = 20000, currentcount = 0}

newGame : Int -> GameState
newGame rnd = {rows = initialRows rnd, score = 0, countdown = 20000, currentcount = 0}

setState : Int -> Int -> Int -> GameState -> GameState
setState x y rv r =
       if x == 1 then swipeAndAdd RightToLeft r rv
           else if x == -1 then swipeAndAdd LeftToRight r rv
               else if y == 1 then swipeAndAdd TopToBottom r rv
                   else if y == -1 then swipeAndAdd BottomToTop r rv
                       else if r.currentcount <= 0
                            then swipeAndAdd NoSwipe r rv
                            else {r | currentcount = r.currentcount - 1} --,
--                                   countdown <- diff }

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
        rnd = (round rv) % 100
    in
        if ss then newGame rnd else setState x y rnd gameState
--        if ss then newGame rnd else setState x y rnd diff gameState

showVal : String -> Int -> Element
showVal label score =
    let
        g = group [rect 100 50 |> filled lightGrey,
                   toForm (flow down [fromString label |> centered,
                                      toString score |> fromString |> rightAligned])]
    in
        collage 100 50 [g]

showScore : Int -> Element
showScore score = showVal "Score" score

showCountdown : Int -> Element
showCountdown count = showVal "Countdown" count

gameOverStyle = {defaultStyle | bold = True,
                                height = Just 24,
                                color = red }

display : (Int, Int) -> GameState -> Element
display (w,h) gs = container w h middle (flow down [ flow right [if gs.countdown < 100 then showCountdown gs.currentcount
                                                                                       else spacer 100 50,
                                                                 spacer 50 50,
                                                                 --difficultyDropdown,
                                                                 spacer 50 50,
                                                                 showScore gs.score],
                                                     (grid 400 gs.rows),
                                                     if gameOver gs then fromString "Game Over" |> style gameOverStyle  |> centered
                                                                    else fromString "" |> centered ])
gameState = foldp stepGame defaultGame userInput
