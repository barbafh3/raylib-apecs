{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Apecs
import Building.Warehouse (newWarehouse)
import Components
import Control.Monad (unless)
import Data.Data (typeOf)
import qualified Data.HashMap as Map
import Debug.Trace (traceShow)
import Draw (drawGame)
import Foreign.C.Types (CFloat (..))
import GHC.Base (when)
import GHC.Float (int2Float)
import Input
import Linear (V2 (..))
import Raylib
  ( beginDrawing,
    clearBackground,
    closeWindow,
    drawCircle,
    drawText,
    drawTextEx,
    endDrawing,
    initWindow,
    isKeyDown,
    isKeyPressed,
    isKeyReleased,
    isKeyUp,
    loadFont,
    loadTexture,
    setTargetFPS,
    windowShouldClose,
  )
import Raylib.Colors (black, rayWhite, white)
import Raylib.Constants (key'a, key'd, key'o, key's, key'up, key'w, mouseButton'left, mouseButton'right)
import Raylib.Types (Camera2D (..), Font, Rectangle (..), Texture, Vector2 (..))
import Tilemap (generateTilemap, tileSizeCF)
import UI (newLabel, uiStartup)
import Update (updateGame)
import Villager.Hauler (newHauler)

tilesetPath, uiAtlasPath, mainFontPath :: String
tilesetPath = "assets/tileset.png"
uiAtlasPath = "assets/ui.png"
mainFontPath = "assets/prstartk.ttf"

-- mainFontPath = "assets/Minecraft.ttf"

gameKeyboardActions :: [KeyboardAction]
gameKeyboardActions =
  [ KeyboardAction MoveLeft [key'a] Up,
    KeyboardAction MoveRight [key'd] Up,
    KeyboardAction MoveDown [key's] Up,
    KeyboardAction MoveUp [key'w, key'up] Up,
    KeyboardAction AddStone [key'o] Up
  ]

gameMouseActions :: [MouseAction]
gameMouseActions =
  [ MouseAction LeftClick [mouseButton'left] Up,
    MouseAction RightClick [mouseButton'right] Up
  ]

main :: IO ()
main = initWorld' >>= runSystem initializeGame

screenWidth :: Int
screenWidth = 1280

screenHeight :: Int
screenHeight = 720

screenWidthCF :: CFloat
screenWidthCF = CFloat $ int2Float screenWidth

screenHeightCF :: CFloat
screenHeightCF = CFloat $ int2Float screenHeight

initializeGame :: System' ()
initializeGame = do
  liftIO $ do
    initWindow screenWidth screenHeight "Protobuilder"
    setTargetFPS 75

  setupGlobalInputActions gameKeyboardActions gameMouseActions

  mapAtlas <- liftIO $ loadTexture tilesetPath
  uiAtlas <- liftIO $ loadTexture uiAtlasPath
  set global $ GameAtlasSets mapAtlas uiAtlas

  mainFont <- liftIO $ loadFont mainFontPath
  set global $ FontsComponent mainFont

  generateTilemap 1024 1024

  set global $ ShowFPS False
  set global $ DrawCollisions False

  set global $ GlobalStorageList $ Map.fromList [("Wood", 50)]

  let camera = Camera2D (Vector2 0.0 0.0) (Vector2 0.0 0.0) 0.0 2.0
  set global $ CameraComponent 10.0 camera

  uiStartup

  let idlePoint = Vector2 304.0 176.0

  -- Flag Sprite at IdlePoint
  _ <- newEntity (Sprite idlePoint (Rectangle (2.0 * tileSizeCF) (6.0 * tileSizeCF) tileSizeCF tileSizeCF))

  _ <- newHauler (Vector2 250.0 50.0) idlePoint Trigger
  _ <- newHauler (Vector2 250.0 250.0) idlePoint Trigger

  _ <- newWarehouse (Vector2 208.0 48.0) (Just $ Map.singleton "Wood" 50)

  gameLoop mainFont
  liftIO closeWindow

gameLoop :: Font -> System' ()
gameLoop mainFont = do
  handleInput
  updateGame

  liftIO $ do
    beginDrawing
    clearBackground rayWhite

  drawGame

  liftIO endDrawing

  shouldClose <- liftIO windowShouldClose
  unless shouldClose $ gameLoop mainFont
