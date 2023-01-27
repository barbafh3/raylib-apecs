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
import Constants (screenHeight, screenWidth)
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
import Raylib.Types (Camera2D (..), Font, Rectangle (..), Texture, Vector2 (..))
import Startup (gameStartup)
import Tilemap (generateTilemap, tileSizeCF)
import UI (newLabel)
import Update (updateGame)
import Villager.Hauler (newHauler)

main :: IO ()
main = initWorld' >>= runSystem initializeGame

initializeGame :: System' ()
initializeGame = do
  liftIO $ do
    initWindow screenWidth screenHeight "Protobuilder"
    setTargetFPS 75

  mainFont <- gameStartup
  gameLoop mainFont
  liftIO closeWindow

gameLoop :: Font -> System' ()
gameLoop mainFont = do
  handleInput

  (ActiveScene scene) <- get global
  updateGame scene

  liftIO $ do
    beginDrawing
    clearBackground rayWhite

  drawGame

  liftIO endDrawing

  shouldClose <- liftIO windowShouldClose
  unless shouldClose $ gameLoop mainFont
