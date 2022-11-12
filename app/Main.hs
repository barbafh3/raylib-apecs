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
import Raylib.Types (Camera2D(..), Vector2 (..), Texture)
import Raylib.Constants (key'w, key'd, key's, key'a, key'up)
import GHC.Base (when)
import Input
import Components (System', Position (..), initWorld', InputAction (..), InputState (..))
import Raylib.Colors (rayWhite)
import Update (updateGame)
import Draw (drawGame)
import Tilemap (generateTilemap)

tilesetPath :: String
tilesetPath = "assets/tileset.png"

gameInputActions :: [InputAction]
gameInputActions = [
    InputAction "Left" [key'a] Up,
    InputAction "Right" [key'd] Up,
    InputAction "Down" [key's] Up,
    InputAction "Up" [key'w, key'up] Up
  ]


main :: IO ()
main = initWorld' >>= runSystem game

game :: System' ()
game = do 
  setupInputActions gameInputActions
  _ <- newEntity $ Position 1
  liftIO $ do 
    initWindow 1280 720 "Protobuilder"
    setTargetFPS 60

  tileset <- liftIO $ loadTexture tilesetPath
  generateTilemap

  let camera = Camera2D (Vector2 0.0 0.0) (Vector2 0.0 0.0) 0.0 2.0
  gameLoop tileset camera
  liftIO closeWindow

gameLoop :: Texture -> Camera2D -> System' ()
gameLoop tileset camera = do
  handleInput
  updateGame

  liftIO $ do 
    beginDrawing
    clearBackground rayWhite

  drawGame tileset camera

  liftIO endDrawing

  shouldClose <- liftIO windowShouldClose
  unless shouldClose (gameLoop tileset camera)

