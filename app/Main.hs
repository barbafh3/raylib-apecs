{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Apecs
import Components
import Control.Monad (unless)
import Data.Data (typeOf)
import Debug.Trace (traceShow)
import Draw (drawGame)
import GHC.Base (when)
import Hauler (newHauler)
import Input
import Linear (V2 (..))
import Raylib
  ( beginDrawing,
    clearBackground,
    closeWindow,
    drawText,
    endDrawing,
    initWindow,
    isKeyDown,
    isKeyPressed,
    isKeyReleased,
    isKeyUp,
    loadTexture,
    setTargetFPS,
    windowShouldClose,
  )
import Raylib.Colors (rayWhite)
import Raylib.Constants (key'a, key'd, key's, key'up, key'w, mouseButton'left, mouseButton'right)
import Raylib.Types (Camera2D (..), Rectangle (..), Texture, Vector2 (..))
import Tilemap (generateTilemap, tileSizeCF)
import UI (uiStartup)
import Update (updateGame)

tilesetPath, uiAtlasPath :: String
tilesetPath = "assets/tileset.png"
uiAtlasPath = "assets/ui.png"

gameKeyboardActions :: [KeyboardAction]
gameKeyboardActions =
  [ KeyboardAction MoveLeft [key'a] Up,
    KeyboardAction MoveRight [key'd] Up,
    KeyboardAction MoveDown [key's] Up,
    KeyboardAction MoveUp [key'w, key'up] Up
  ]

gameMouseActions :: [MouseAction]
gameMouseActions =
  [ MouseAction LeftClick [mouseButton'left] Up,
    MouseAction RightClick [mouseButton'right] Up
  ]

main :: IO ()
main = initWorld' >>= runSystem initializeGame

initializeGame :: System' ()
initializeGame = do
  liftIO $ do
    initWindow 1280 720 "Protobuilder"
    setTargetFPS 75

  setupGlobalInputActions gameKeyboardActions gameMouseActions

  mapAtlas <- liftIO $ loadTexture tilesetPath
  uiAtlas <- liftIO $ loadTexture uiAtlasPath
  set global $ GameAtlasSets mapAtlas uiAtlas

  generateTilemap 1024 1024

  set global $ ShowFPS False
  set global $ DrawCollisions False

  uiStartup

  let camera = Camera2D (Vector2 0.0 0.0) (Vector2 0.0 0.0) 0.0 2.0
  set global $ CameraComponent 10.0 camera

  let idlePoint = Vector2 50.0 50.0

  _ <- newHauler (Vector2 10.0 10.0) idlePoint
  _ <- newHauler (Vector2 70.0 70.0) idlePoint

  gameLoop
  liftIO closeWindow

gameLoop :: System' ()
gameLoop = do
  handleInput
  updateGame

  liftIO $ do
    beginDrawing
    clearBackground rayWhite

  drawGame

  liftIO endDrawing

  shouldClose <- liftIO windowShouldClose
  unless shouldClose gameLoop
