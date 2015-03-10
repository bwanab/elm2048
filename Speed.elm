import Graphics.Element (..)
import Graphics.Collage (group,rect,filled,toForm,collage,circle,move)
import Color (lightGrey,red,blue)
import Text (plainText, fromString, rightAligned, defaultStyle, style, centered)
import Signal (map2, map4,foldp)
import Keyboard
import Window
import Random
import Time(..)

type alias V = {x : Float, y : Float , z : Float}
type alias State = {position : V, speed : V, accel : V}

m : State
m = {position = {x = 0, y = 0, z = 0}, speed = {x = 0, y = 0, z = 0}, accel = {x = 0, y = 0, z = 0}}

addV : V -> V -> V
addV v1 v2 = {x = v1.x + v2.x, y = v1.y + v2.y, z = v1.z + v2.z }

addBuffeting : Float -> Float -> Float -> Float
addBuffeting val rand speed = val + speed * (rand - 0.5)

testBuffet : Float -> Bool
testBuffet r = 0 == ((round (r * 100)) % 2)

-- Update
update : (Time, {x: Int, y: Int}, Bool, Bool) -> State -> State
update (t, {x,y}, forward, brake) state =
   let
        newForward = if forward then 1 else 0
        newBrake = if brake then -1 else 0
        newAccel : V
        newAccel = {x = toFloat x, y = toFloat y, z = newForward + newBrake}
        newSpeed = addV newAccel state.speed
        tPosition = addV state.position newSpeed
        rand = 100
        tB = testBuffet rand
        newPosition = {tPosition | x <- if tB then addBuffeting tPosition.x rand newSpeed.z else tPosition.x,
                                   y <- if not tB then addBuffeting tPosition.y rand newSpeed.z else tPosition.y }
   in
        {state | speed <- newSpeed, position <- newPosition, accel <- newAccel }

format1 : String -> Float -> String
format1 s v = s ++ ": " ++ toString (round v) ++ " "

formatV : V -> String
formatV {x,y,z} = format1 "x" x ++ format1 "y" y ++ format1 "z" z

showV : String -> V -> Element
showV label v =
    let
        g = group [rect 200 50 |> filled lightGrey,
                   toForm (flow down [plainText label,
                                      formatV v |> fromString |> rightAligned])]
    in
          collage 200 50 [g]

display : Int -> Int -> State -> Element
display w h m = collage w h [ rect (toFloat w) (toFloat h) |> filled lightGrey,
                              circle 10 |> filled blue |> move (m.position.x, m.position.y) ]

render : (Int, Int) -> State -> Element
render (w,h) m = container w h middle (flow down [flow right [showV "Position" m.position,
                                                              showV "Speed" m.speed,
                                                              showV "Accel" m.accel],
                                                 display w (h - 50) m])
input =
  map4 (,,,)
        (foldp (\t acc -> acc + (t/100)) 0 (fps 24))
        Keyboard.arrows
        Keyboard.space
        Keyboard.ctrl
        --(Random.float 0 (fps 24))

-- Main
main = map2 render Window.dimensions <| foldp update m input
