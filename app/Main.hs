module Main where

import Bot
  ( botControlDirection,
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
    generateNewFood,
    initialGameState,
    move,
  )

window :: Display
window = InWindow "Haskell Snake Game" (700, 560) (100, 100)

background :: Color
background = white

render :: GameState -> Picture
render gameState =
  pictures $
    [ fillRectangle black (16, 0) (640, 20),
      fillRectangle black (16, 24) (640, 20),
      fillRectangle black (0, 12) (20, 480),
      fillRectangle black (32, 12) (20, 480)
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
    fillRectangle color' (tx, ty) (w, h) =
      color color' $
        scale 1 (-1) $
          translate (tx * 20 - 320) (ty * 20 - 240) $
            rectangleSolid w h
    toFloat (x, y) = (fromIntegral x, fromIntegral y)
    foodCounterPicture =
      [ translate 150 200 $scale 0.2 0.2 $ text "Snake length:",
        translate 250 200 $scale 0.2 0.2 $ text . show $ length snake
      ]
    modePicture =
      if isBotMode gameState
        then
          [ translate 100 100 $scale 0.2 0.2 $
              text "Bot"
          ]
        else
          [ translate 100 100 $scale 0.2 0.2 $
              text
                "Player"
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
    else GameState newSnake newFood' direction newGameOver newStdGen botGameMode
  where
    snake = getSnake gameState
    food = getFood gameState
    botGameMode = isBotMode gameState
    direction =
      if botGameMode
        then botControlDirection gameState
        else getDirection gameState
    gameOver = isGameOver gameState
    stdGen = getRandomStdGen gameState
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
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) gameState = changeMode gameState $ not (isBotMode gameState)
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