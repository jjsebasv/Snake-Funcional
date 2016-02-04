-- View

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)

import List exposing (map)
import Maybe
import Text

-- Signals

import Char
import Keyboard
import Random
import Signal exposing (Signal, filter, mergeMany)
import Time exposing (Time, fps)

-- Snake State

import List exposing (head)
import Maybe exposing (withDefault)
import List exposing ((::), drop, head, isEmpty, map, reverse)
import Set
import Signal exposing (Signal, foldp)

{--
toForm : Element -> Form
    Turn any Element into a Form. 
    This lets you use text, gifs, and video in your collage. 
    This means you can move, rotate, and scale an Element however you want.

collage : Int -> Int -> List Form -> Element

show : a -> Element
--}

-- View

type alias Position =
  { x: Int
  , y: Int
  }

type alias Direction =
  { dx: Int
  , dy: Int
  }

type alias Snake =
  { head: List Position
  , tail: List Position
  }

type SnakeParts = Head | Body | Tail

type alias SnakeState =
    { snake: Snake
    , delta: Direction
    , food: Maybe Position
    , ticks: Int
    , gameOver: Bool
    }

initialDelta = { dx = 0, dy = 1 }

initialFood = Nothing

initialState =
  { snake = initialSnake
  , delta = initialDelta
  , food = initialFood
  , ticks = 0
  , gameOver = False
  }

initialSnake =
  { head = [{x = 0, y = -1}, {x = 0, y = -2}, {x = 0, y = -3}]
  , tail = [{x = 0, y = -4}]
  }

unit = 10
boxSize = 40
boardSize = 15
velocity = 15

innerSize = unit * boxSize

outerSize = unit * (boxSize+1)

board = collage outerSize outerSize [ filled black
                                              <| rect outerSize outerSize,
                                            filled white
                                              <| rect innerSize innerSize
                                    ]

drawSquare : Color -> Position -> Form
drawSquare color position =
  filled color (rect unit unit)
    |> move (toFloat (unit*position.x), toFloat (unit*position.y))

drawSnake : Color -> List Position -> Element
drawSnake color points =
  collage outerSize outerSize (map (drawSquare color) points) 


view state =
    layers [ board
           , drawSnake green state.snake.head
           , drawSnake red state.snake.tail
           ]

-- Signals

type Event = Tick Position | DirectionEvent Direction | NewGame | Ignore | PauseGame

timeSignal : Signal Time
timeSignal = fps 50

makeTick : Time -> Event
makeTick time =
  let seed1 = Random.initialSeed (round time)
      (x,seed2) = Random.generate (Random.int -boardSize boardSize) seed1
      (y,_) = Random.generate (Random.int -boardSize boardSize) seed2
  in
    Tick { x = x, y = y }

tickSignal : Signal Event
tickSignal = 
  Signal.map makeTick timeSignal

directionSignal : Signal Event
directionSignal =
  let arrowsToDelta {x,y} =
    if x == 0 && y == 0 then
      Ignore
    else if x /= 0 then
        DirectionEvent { dx = x, dy = 0 }
      else
        DirectionEvent { dx = 0, dy = y }          
  in
    Signal.map arrowsToDelta Keyboard.arrows

{--

newGameSignal : Signal Event
newGameSignal =
  NewGame (filter identity False <| Keyboard.isDown (Char.toCode 'N'))

pauseGameSignal : Signal Event
pauseGameSignal =
  Signal.map NewGame (filter identity False <| Keyboard.isDown (Char.toCode 'P'))

--}

eventSignal : Signal Event
eventSignal =
  mergeMany [tickSignal, directionSignal]

-- Snake State

nextPosition : Snake -> Direction -> Position
nextPosition snake {dx,dy} =
    let headPosition = head snake.head |> withDefault {x=0,y=0}
    in  { x = headPosition.x + dx, y = headPosition.y + dy }

collision : SnakeState -> Bool 
collision state = 
  let next = nextPosition state.snake state.delta
  in
    if abs next.x > boardSize || abs next.y > boardSize || isInSnake state.snake next
    then True
    else False

isInSnake : Snake -> Position -> Bool
isInSnake snake position =
  let frontSet = Set.fromList <| map toString snake.head
      backSet = Set.fromList <| map toString snake.tail
  in
    Set.member (toString position) frontSet || Set.member (toString position) backSet

moveSnakeForward : Snake -> Direction -> Maybe Position -> Snake
moveSnakeForward snake delta food =
  let next = nextPosition snake delta
      tailFunction =
        case food of
          Nothing -> drop 1
          Just f -> if next == f then identity else drop 1
  in
    if isEmpty snake.tail
    then { head = [next]
         , tail = (tailFunction << reverse) snake.head }
    else { head = next :: snake.head
         , tail = tailFunction snake.tail }

step : Event -> SnakeState -> SnakeState
step event state =
    case (event,state.gameOver) of
        (NewGame,_) -> initialState
        (PauseGame,_) -> state
        (_,True) -> state
        (DirectionEvent newDelta,_) ->
            { state | delta = if abs newDelta.dx /= abs state.delta.dx
                               then newDelta
                               else state.delta }
        (Tick newFood, _) ->
            let state1 = if state.ticks % velocity == 0
                         then { state | gameOver = collision state }
                         else state
            in if state1.gameOver
               then state1
               else let state2 = { state1
                                  | snake =
                                      if state1.ticks % velocity == 0
                                      then moveSnakeForward state1.snake state1.delta state1.food
                                      else state1.snake
                                  }
                        state3 = { state2
                                 | food =
                                       case state2.food of
                                           Just f -> 
                                               if state2.ticks % velocity == 0 &&
                                                  head state2.snake.head == Just f
                                               then Nothing
                                               else state2.food
                                           Nothing ->
                                               if isInSnake state2.snake newFood
                                               then Nothing
                                               else Just newFood
                                 }
                     in { state3 | ticks = state3.ticks + 1 }
        (Ignore,_) -> state

stateSignal : Signal SnakeState
stateSignal = foldp step initialState eventSignal

-- Main

main : Signal Element
main = 
  Signal.map view stateSignal
