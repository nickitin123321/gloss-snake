module Snake
  ( Direction (..),
    GameState (..),
    Snake,
    ClosedPath,
    changeDirection,
    changeMode,
    checkGameOver,
    generateNewFood,
    initialGameState,
    move,
    Point,
    cols,
    rows,
    (+:),
    isPathContain,
    firstWallsPoint,
    getHamPath,
  )
where

import Data.Map (Map, fromList, (!))
import System.Random (Random (randomR), StdGen, mkStdGen)

type Point = (Int, Int)

type Snake = [Point]

-- Enum of directions.
data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord, Show)

-- Operator increases tuple values.
(+:) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) +: (c, d) = (a + c, b + d)

cols :: Int
cols = 32

rows :: Int
rows = 24

initWalls :: ((Int, Int), (Int, Int))
initWalls = ((1, 1), (cols, rows))

clockwise :: [Direction]
clockwise = [UP, RIGHT, DOWN, LEFT]

directionVectorMap :: Map Direction (Int, Int)
directionVectorMap = fromList [(UP, (0, - step)), (DOWN, (0, step)), (LEFT, (- step, 0)), (RIGHT, (step, 0))]
  where
    step = 1

move :: Point -> Direction -> Snake -> (Bool, Snake)
move food direction snake =
  if wasFoodEaten
    then (True, newHead : snake)
    else (False, newHead : init snake)
  where
    wasFoodEaten = newHead == food
    head' = head snake
    -- ! - Find the value at a key of map
    newHead = directionVectorMap ! direction +: head'

-- Operator, which increase coordinate on direction value

-- If head in tail or walls, then true.
checkGameOver :: Snake -> Bool
checkGameOver snake =
  headX == 1
    || headY == 1
    || headX == cols
    || headY == rows
    || head' `elem` tail'
  where
    head' = head snake
    (headX, headY) = head'
    tail' = tail snake

generateNewFood :: Snake -> StdGen -> (Point, StdGen)
generateNewFood snake stdGen =
  if newFood `elem` snake
    then generateNewFood snake stdGen3
    else (newFood, stdGen3)
  where
    (foodX, stdGen2) = randomR (2, cols - 1) stdGen
    (foodY, stdGen3) = randomR (2, rows - 1) stdGen2
    newFood = (foodX, foodY)

data GameState = GameState
  { getSnake :: Snake,
    getFood :: Point,
    getDirection :: Direction,
    isGameOver :: Bool,
    getRandomStdGen :: StdGen,
    isBotMode :: Bool,
    hamGetter :: ClosedPath
  }

changeDirection :: GameState -> Direction -> GameState
changeDirection gameState newDir =
  if direction == UP && newDir == DOWN
    || direction == DOWN && newDir == UP
    || direction == LEFT && newDir == RIGHT
    || direction == RIGHT && newDir == LEFT
    then gameState
    else GameState snake square newDir isGameOver stdGen botMode ham
  where
    (GameState snake square direction isGameOver stdGen botMode ham) = gameState

changeMode :: GameState -> Bool -> GameState
changeMode (GameState snake square direction isGameOver stdGen botMode ham) newBotMode =
  GameState snake square direction isGameOver stdGen newBotMode ham

firstWallsPoint :: (Int, Int)
firstWallsPoint = (2, 2)

initialGameState :: Bool -> GameState
initialGameState gameOver =
  GameState
    { getSnake =
        [ (snakeX, snakeY),
          (snakeX, snakeY + 1)
        ],
      getFood = (3, 3),
      getDirection = DOWN,
      isGameOver = gameOver,
      getRandomStdGen = randomStdGen,
      isBotMode = False,
      hamGetter = getHamPath firstWallsPoint []
    }
  where
    randomStdGen = mkStdGen 100
    (snakeX, snakeY) = (5, 5)

-- Checks collision snake with wall.
collisionWall :: (Ord a1, Ord a2) => (a1, a2) -> ((a1, a2), (a1, a2)) -> Bool
collisionWall (sx, sy) ((wx1, wy1), (wx2, wy2)) =
  sx <= wx1 || sx >= wx2 || sy <= wy1 || sy >= wy2

type ClosedPath = [Point]

getHamPath :: Point -> ClosedPath -> ClosedPath
getHamPath currentPoint hamPath
  | hamPathCapacity initWalls == length (currentPoint : hamPath)
      && distBetweenPoints currentPoint (last hamPath) == 1 =
    currentPoint : hamPath
  | otherwise = getHamPath newPoint (currentPoint : hamPath)
  where
    newPoint = nextHamPathPoint (currentPoint : hamPath) clockwise
    hamPathCapacity ((x1, y1), (x2, y2)) = (x2 - x1 - 1) * (y2 - y1 - 1)

type Path = [Point]

nextHamPathPoint :: Path -> [Direction] -> Point
nextHamPathPoint _ [] = error "incorrect initWalls"
nextHamPathPoint hamPath (dir : dirs)
  | isPathContain hamPath virtualPoint
      || collisionWall virtualPoint initWalls =
    nextHamPathPoint hamPath dirs
  | otherwise = virtualPoint
  where
    virtualPoint = directionVectorMap ! dir +: head hamPath

-- isPathContain :: Path -> Point -> Bool
-- isPathContain path point = any( ==point ) path

isPathContain :: Path -> Point -> Bool
isPathContain path point = point `elem` path

distBetweenPoints :: Point -> Point -> Int
distBetweenPoints (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
