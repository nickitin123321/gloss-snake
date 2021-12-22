module Main where

import Bot
  ( PathDirection (DirFromHead, DirFromTail),
    nextDirOnPath,
  )
import Data.Char
import Graphics.Gloss
  ( Color,
    Display (InWindow),
    Picture,
    black,
    blue,
    color,
    pictures,
    play,
    rectangleSolid,
    red,
    scale,
    text,
    translate,
    white,
  )
import Graphics.Gloss.Interface.Pure.Game
  ( Color,
    Display (InWindow),
    Event (EventKey),
    Key (SpecialKey),
    KeyState (Down),
    Picture,
    SpecialKey (KeyDown, KeyEnter, KeyLeft, KeyRight, KeySpace, KeyUp),
    black,
    blue,
    color,
    green,
    pictures,
    play,
    rectangleSolid,
    scale,
    text,
    translate,
    white,
  )
import Snake
  ( Direction (..),
    GameState (..),
    Snake,
    changeDirection,
    changeMode,
    checkGameOver,
    firstWallsPoint,
    generateNewFood,
    getHamPath,
    initialGameState,
    move,
  )

window :: Display
window = InWindow "Haskell Snake Game" (720, 560) (100, 100)

background :: Color
background = white

render :: GameState -> Picture
render gameState =
  pictures $
    [ -- Top wall.
      fillRectangle black (17, 1) (620, 20),
      -- Bottom wall.
      fillRectangle black (17, 24) (620, 20),
      -- Left wall.
      fillRectangle black (1, 12) (20, 460),
      -- Right wall.
      fillRectangle black (32, 12) (20, 460)
      ,
      fillRectangle black (1, 24) (20, 20)
    ]
      ++ fmap (convertToPicture green) tail'
      ++ [convertToPicture blue head']
      ++ [convertToPicture red food]
      ++ gameOverPicture
      ++ foodCounterPicture
      ++ modePicture
  where
    tail' = tail snake
    head' = head snake
    snake = getSnake gameState
    food = getFood gameState
    convertToPicture :: Color -> (Int, Int) -> Picture
    convertToPicture color' (x, y) = fillRectangle color' (toFloat (x, y)) (20, 20)
    fillRectangle :: Color -> (Float, Float) -> (Float, Float) -> Picture
    fillRectangle color' (tx, ty) (width, height) =
      color color' $
        scale 1 (-1) $
          translate (tx * 20 - 320) (ty * 20 - 240) $
            rectangleSolid width height
    toFloat (x, y) = (fromIntegral x, fromIntegral y)
    foodCounterPicture =
      [ translate 80 150 $ scale 0.2 0.2 $ text "Snake length:",
        translate 250 150 $ scale 0.2 0.2 $ text $ show snakeLength
      ]
    snakeLength = length snake
    modePicture =
      if isBotMode gameState
        then
          [ translate 100 100 $
              scale 0.2 0.2 $
                text "Mode: Bot"
          ]
        else
          [ translate 100 100 $
              scale 0.2 0.2 $
                text
                  "Mode: Player"
          ]
    gameOverPicture =
      if isGameOver gameState
        then
          [ color blue
              . translate (-200) 0
              . scale 0.5 0.5
              . text
              $ "GAME OVER",
            color blue $
              translate (-175) (-50)
                . scale 0.2 0.2
                . text
                $ "Press SPACE to try again."
          ]
        else []

update :: Float -> GameState -> GameState
update seconds gameState =
  if gameOver
    then gameState
    else GameState newSnake newFood' direction newGameOver newStdGen botGameMode ham
  where
    snake = getSnake gameState
    food = getFood gameState
    botGameMode = isBotMode gameState
    ham = if botPathDir == DirFromTail then hamGetter gameState else reverse $ hamGetter gameState
    direction = if botGameMode then botStepDir else getDirection gameState
    gameOver = isGameOver gameState
    stdGen = getRandomStdGen gameState
    (botStepDir, botPathDir) = nextDirOnPath snake (hamGetter gameState)
    (wasFoodEaten, newSnake) = move food direction snake
    (newFood, newStdGen) = generateNewFood newSnake stdGen
    newFood' =
      if wasFoodEaten
        then newFood
        else food
    newGameOver = checkGameOver newSnake

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gameState = changeDirection gameState LEFT
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gameState = changeDirection gameState RIGHT
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) gameState = changeDirection gameState UP
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) gameState = changeDirection gameState DOWN
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) gameState = changeMode gameState $ not $ isBotMode gameState
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) gameState =
  if isGameOver gameState
    then initialGameState False
    else gameState
handleKeys _ gameState = gameState

main :: IO ()
main = play window background speed (initialGameState True) render handleKeys update
  where
    -- 10 is number of simulation steps to take for each second of real time
    speed = 10