import Tiles (Tile, swipeAndAdd, RightToLeft, LeftToRight, TopToBottom, BottomToTop, initialRows)
import Window
import Keyboard

userInput : Signal
userInput = Keyboard.arrows


type GameState = [[Tile]]
defaultGame : GameState
defaultGame = initialRows

setState x r = if | x.x == -1 -> swipeAndAdd RightToLeft r
                  | x.x == 1 -> swipeAndAdd LeftToRight r
                  | x.y == -1 -> swipeAndAdd TopToBottom r
                  | x.y == 1 -> swipeAndAdd BottomToTop r
                  | otherwise -> r

stepGame : Input -> GameState -> GameState
stepGame userInput gameState = setState userInput gameState

display : (Int, Int) -> GameState -> Element
display (w,h) gameState = grid w gameState

input lift Input userInput

gameState = foldp stepGame defaultGame input

main : Signal : Element
main = lift2 display Window.dimensions gameState
