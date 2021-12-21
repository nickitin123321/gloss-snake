module Bot (nextDirOnPath, PathDirection (DirFromHead, DirFromTail)) where

import Snake
  ( ClosedPath,
    Direction (..),
    GameState (..),
    Point,
    Snake,
    changeDirection,
    checkGameOver,
    cols,
    generateNewFood,
    initialGameState,
    isPathContain,
    move,
    rows,
    (+:),
  )

data PathDirection = DirFromHead | DirFromTail deriving (Eq, Show)

-- botControlDirection :: GameState -> Direction
-- botControlDirection gameState
--   | foodX == headX = if foodY > headY then DOWN else UP
--   | foodY == headY = if foodX > headX then RIGHT else LEFT
--   | headX == cols - stepBeforeWall && snakeX /= cols - stepBeforeWall
--       || headY == rows - stepBeforeWall && snakeY /= rows - stepBeforeWall
--       || headY == stepBeforeWall && snakeY /= stepBeforeWall
--       || headX == stepBeforeWall && snakeX /= stepBeforeWall =
--     --
--     case direction of
--       LEFT -> if headY == rows - stepBeforeWall then UP else DOWN
--       UP -> if headX == stepBeforeWall then RIGHT else LEFT
--       RIGHT -> if headY == stepBeforeWall then DOWN else UP
--       DOWN -> if headY == rows - stepBeforeWall then LEFT else RIGHT
--   | otherwise = direction
--   where
--     snake = getSnake gameState
--     direction = getDirection gameState
--     (foodX, foodY) = getFood gameState
--     head' = head snake
--     (headX, headY) = head'
--     (snakeX, snakeY) = head $ tail snake
--     stepBeforeWall = 1
--     nextHead = (1, 1) +: head'

-- botControlDirection :: GameState -> Direction
-- botControlDirection gameState = dir
--   where
--     snake = getSnake gameState
--     ham = if botPathDir == DirFromTail then hamPath gameState else reverse $ hamPath gameState
--     (dir, botPathDir) = nextDirOnPath snake ham

pointNeighborsOnPath :: Point -> ClosedPath -> (Point, Point)
pointNeighborsOnPath point path
  | not $ isPathContain path point || length path < 4 = error "incorrect initWalls"
  | point == head path = (last path, head $ tail path)
  | point == last path = (last $ init path, head path)
  | otherwise = _pointNeighborsOnPath point path
  where
    _pointNeighborsOnPath point (a : b : c : xs) =
      if point == b
        then (a, c)
        else _pointNeighborsOnPath point (b : c : xs)

nextDirOnPath :: Snake -> ClosedPath -> (Direction, PathDirection)
nextDirOnPath (snakeHead : snakeTail) path
  | snakeTail == [] = (dirBetweenPoints snakeHead point1, DirFromTail)
  | point1 == head snakeTail = (dirBetweenPoints snakeHead point2, DirFromHead)
  | otherwise = (dirBetweenPoints snakeHead point1, DirFromTail)
  where
    (point1, point2) = pointNeighborsOnPath snakeHead path

dirBetweenPoints :: Point -> Point -> Direction
dirBetweenPoints (x1, y1) (x2, y2)
  | x1 == x2 = if y1 > y2 then UP else DOWN
  | y1 == y2 = if x1 > x2 then LEFT else RIGHT
  | otherwise =
    if abs (x1 - x2) < abs (y1 - y2)
      then dirBetweenPoints (x1, 0) (x2, 0)
      else dirBetweenPoints (0, y1) (0, y2)
