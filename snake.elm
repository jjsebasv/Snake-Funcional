import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)

import List exposing (map)
import Maybe
import Text

{--
toForm : Element -> Form
    Turn any Element into a Form. 
    This lets you use text, gifs, and video in your collage. 
    This means you can move, rotate, and scale an Element however you want.

collage : Int -> Int -> List Form -> Element

show : a -> Element
--}

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
  { head = [{x = 0, y = 0}, {x = 0, y = -1}, {x = 0, y = -2}, {x = 0, y = -3}]
  , tail = []
  }

unit = 10
boxSize = 40

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
           , drawSnake blue state.snake.head
           , drawSnake blue state.snake.tail
           ]

-- Main

main =
  view initialState
