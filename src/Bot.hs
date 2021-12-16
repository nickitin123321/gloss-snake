module Bot (botControlDirection, getHamPath, wallsFirstPoint) where

import Snake
  ( Direction (..),
    GameState (..),
    Snake,
    Point,
    changeDirection,
    checkGameOver,
    cols,
    generateNewFood,
    initialGameState,
    move,
    rows,
    (+:),
  )

type Path = [Point]

type ClosedPath = [Point]

data PathDirection = DirFromHead | DirFromTail deriving (Eq)

botControlDirection :: GameState -> Direction
botControlDirection gameState
  | foodX == headX = if foodY > headY then DOWN else UP
  | foodY == headY = if foodX > headX then RIGHT else LEFT
  | headX == cols - stepBeforeWall && snakeX /= cols - stepBeforeWall
      || headY == rows - stepBeforeWall && snakeY /= rows - stepBeforeWall
      || headY == stepBeforeWall && snakeY /= stepBeforeWall
      || headX == stepBeforeWall && snakeX /= stepBeforeWall =
    --
    case direction of
      LEFT -> if headY == rows - stepBeforeWall then UP else DOWN
      UP -> if headX == stepBeforeWall then RIGHT else LEFT
      RIGHT -> if headY == stepBeforeWall then DOWN else UP
      DOWN -> if headY == rows - stepBeforeWall then LEFT else RIGHT
  | otherwise = direction
  where
    snake = getSnake gameState
    direction = getDirection gameState
    (foodX, foodY) = getFood gameState
    head' = head snake
    (headX, headY) = head'
    (snakeX, snakeY) = head $ tail snake
    stepBeforeWall = 1
    nextHead = (1, 1) +: head'

initWalls :: ((Int, Int), (Int, Int))
initWalls = ((0, 0), (32, 32))

clockwise :: [Direction]
clockwise = [UP, DOWN, LEFT, RIGHT]

wallsFirstPoint :: Point
wallsFirstPoint = (cols + 1, rows + 1)

isPathContain :: Path -> Point -> Bool
isPathContain path point = any (== point) path

distBetweenPoints :: Point -> Point -> Int
distBetweenPoints (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

getHamPath :: Point -> ClosedPath -> ClosedPath
getHamPath currentPoint hamPath
  | hamPathCapacity initWalls == length (currentPoint : hamPath)
      && distBetweenPoints currentPoint (last hamPath) == 1 =
    currentPoint : hamPath
  | otherwise = getHamPath newPoint (currentPoint : hamPath)
  where
    newPoint = nextHamPathPoint (currentPoint : hamPath) clockwise
    hamPathCapacity ((x1, y1), (x2, y2)) = (x2 - x1 - 1) * (y2 - y1 - 1)

nextHamPathPoint :: Path -> [Direction] -> Point
nextHamPathPoint _ [] = error "incorrect initWalls"
nextHamPathPoint hamPath (dir : dirs)
  | isPathContain hamPath virtualPoint
      || checkGameOver virtualPath =
    nextHamPathPoint hamPath dirs
  | otherwise = virtualPoint
  where
    (_, virtualPath) = move (3, 3) DOWN hamPath
    virtualPoint = head virtualPath
