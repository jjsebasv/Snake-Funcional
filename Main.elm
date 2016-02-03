import Text exposing (..)
import Color exposing (..)
import Mouse
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window

diamond color size =
  square size 
    |> filled color 
    |> rotate (degrees 45)

mySquare color size =
  square size
  |> filled color

myMouse : Signal Element
myMouse =
  Signal.map show Mouse.position
  

-- MODEL

type alias Model =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , dir : Direction
  }


type Direction = Left | Right


type alias Keys = { x:Int, y:Int }


customSquare : Model
customSquare =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , dir = Right
  }

-- Squares

headSquare = mySquare blue 30

tailSquare = mySquare red 30

mySnake = flow right [show headSquare, show tailSquare]


-- UPDATE

update : (Float, Keys) -> Model -> Model
update (dt, keys) customSquare =
  customSquare
    --|> gravity dt
    --|> jump keys
    |> walk keys
    |> physics dt



{--
jump : Keys -> Model -> Model
jump keys customSquare =
  if keys.y > 0 && customSquare.vy == 0 then
      { customSquare | vy = 6.0 }

  else
      customSquare

gravity : Float -> Model -> Model
gravity dt customSquare =
  { customSquare |
      vy = if customSquare.y > 0 then customSquare.vy - dt/4 else 0
  }
--}

physics : Float -> Model -> Model
physics dt customSquare =
  let 
    currentY = customSquare.y + dt * customSquare.vy
    currentX = customSquare.x + dt * customSquare.vx
  in
    {
      customSquare |
        x = 
          if currentX > 0  && currentX < 200 then
            currentX
          else if currentX < 0 then 
            1
            else 
              199
        ,y =
          if currentY > 0 && currentY < 200 then
            currentY
          else if currentY < 0 then
            1
            else
              199
    }


walk : Keys -> Model -> Model
walk keys customSquare =
  { customSquare |
      vx = toFloat keys.x * 10,
      vy = toFloat keys.y * 10
  }


-- VIEW

view : (Int, Int) -> Model -> Element
view (w',h') customSquare =
  let
    (w,h) = (toFloat w', toFloat h')

    customSquareImage =
      show mySnake

    groundY = 62 - h/2

    position =
      (customSquare.x, customSquare.y + groundY)
  in
    collage w' h'
      [ {-- Color to background
        rect w h
          |> filled (rgb 174 238 238)
      , rect w 50
          |> filled (rgb 74 167 43)
          |> move (0, 24 - h/2)
      , 
        --}
        mySnake
          |> move position
      ]


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update customSquare input)


input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)

{--
main = 
  show myMouse
  collage 200 300 [toForm (show myMouse)]  
  collage 200 200 [ diamond blue 100, diamond red 75]
--}
