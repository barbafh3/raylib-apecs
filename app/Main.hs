{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main where

import Control.Monad (unless)
import Raylib
  ( beginDrawing,
    clearBackground,
    closeWindow,
    drawText,
    endDrawing,
    initWindow,
    setTargetFPS,
    windowShouldClose, isKeyDown, isKeyPressed, isKeyReleased, isKeyUp, loadTexture,
  )
import Apecs
import Linear (V2 (..))
import Raylib.Types (Camera2D(..), Vector2 (..), Texture, Rectangle (..))
import Raylib.Constants (key'w, key'd, key's, key'a, key'up, mouseButton'left, mouseButton'right)
import GHC.Base (when)
import Input
import Components (System', initWorld', InputAction (..), InputState (..), CameraComponent (..), World (World), ShowFPS (..), BodyCollision (..), CollisionBox (..), DrawCollisions (..))
import Raylib.Colors (rayWhite)
import Update (updateGame)
import Draw (drawGame)
import Tilemap (generateTilemap, tileSizeCF)
import UI (uiStartup)
import Debug.Trace (traceShow)
import Data.Data (typeOf)

tilesetPath, uiAtlasPath :: String
tilesetPath = "assets/tileset.png"
uiAtlasPath = "assets/ui.png"

gameInputActions :: [InputAction]
gameInputActions = [
    InputAction "MoveLeft" [key'a] Up,
    InputAction "MoveRight" [key'd] Up,
    InputAction "MoveDown" [key's] Up,
    InputAction "MoveUp" [key'w, key'up] Up,
    InputAction "LeftClick" [mouseButton'left] Up,
    InputAction "RightClick" [mouseButton'right] Up
  ]

main :: IO ()
main = initWorld' >>= runSystem game

game :: System' ()
game = do 
  liftIO $ do 
    initWindow 1280 720 "Protobuilder"
    setTargetFPS 75

  setupInputActions gameInputActions

  tileset <- liftIO $ loadTexture tilesetPath
  uiAtlas <- liftIO $ loadTexture uiAtlasPath
  generateTilemap 1024 1024

  uiStartup

  let camera = Camera2D (Vector2 0.0 0.0) (Vector2 0.0 0.0) 0.0 2.0

  _ <- newEntity $ CameraComponent 10.0 camera
  _ <- newEntity $ ShowFPS False
  _ <- newEntity $ DrawCollisions False

  _ <- newEntity (BodyCollision False Nothing, CollisionBox (Rectangle 500.0 500.0 tileSizeCF tileSizeCF))
  gameLoop tileset uiAtlas
  liftIO closeWindow

gameLoop :: Texture -> Texture -> System' ()
gameLoop tileset uiAtlas = do
  handleInput
  updateGame

  liftIO $ do 
    beginDrawing
    clearBackground rayWhite

  drawGame tileset uiAtlas

  liftIO endDrawing

  shouldClose <- liftIO windowShouldClose
  unless shouldClose (gameLoop tileset uiAtlas)

