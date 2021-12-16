module Snake
  ( Direction (..),
    GameState (..),
    Snake,
    changeDirection,
    changeMode,
    checkGameOver,
    generateNewFood,
    initialGameState,
    move,
    Square,
    cols,
    rows,
    (+:),
  )
where

import Data.Map (Map, fromList, (!))
import System.Random (Random (randomR), StdGen, mkStdGen)

type Square = (Int, Int)

type Snake = [Square]

data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord)

(+:) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) +: (c, d) = (a + c, b + d)

cols :: Int
cols = 32

rows :: Int
rows = 24

directionVectorMap :: Map Direction (Int, Int)
directionVectorMap = fromList [(UP, (0, - step)), (DOWN, (0, step)), (LEFT, (- step, 0)), (RIGHT, (step, 0))]
  where
    step = 1

move :: Square -> Direction -> Snake -> (Bool, Snake)
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
  headX == 0
    || headX == cols
    || headY == 0
    || headY == rows
    || head' `elem` tail'
  where
    head' = head snake
    (headX, headY) = head'
    tail' = tail snake

generateNewFood :: Snake -> StdGen -> (Square, StdGen)
generateNewFood snake stdGen =
  if newFood `elem` snake
    then generateNewFood snake stdGen3
    else (newFood, stdGen3)
  where
    (foodX, stdGen2) = randomR (1, cols - 1) stdGen
    (foodY, stdGen3) = randomR (1, rows - 1) stdGen2
    newFood = (foodX, foodY)

data GameState = GameState
  { getSnake :: Snake,
    getFood :: Square,
    getDirection :: Direction,
    isGameOver :: Bool,
    getRandomStdGen :: StdGen,
    isBotMode :: Bool
  }

changeDirection :: GameState -> Direction -> GameState
changeDirection gameState newDir =
  if direction == UP && newDir == DOWN
    || direction == DOWN && newDir == UP
    || direction == LEFT && newDir == RIGHT
    || direction == RIGHT && newDir == LEFT
    then gameState
    else GameState snake square newDir isGameOver stdGen botMode
  where
    (GameState snake square direction isGameOver stdGen botMode) = gameState

changeMode :: GameState -> Bool -> GameState
changeMode (GameState snake square direction isGameOver stdGen botMode) newBotMode =
  GameState snake square direction isGameOver stdGen newBotMode

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
      isBotMode = True
    }
  where
    randomStdGen = mkStdGen 100
    (snakeX, snakeY) = (5, 5)
