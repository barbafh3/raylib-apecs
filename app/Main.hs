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
import Raylib.Core (beginDrawing, clearBackground, closeWindow, endDrawing, initWindow, setTargetFPS, windowShouldClose)
import Raylib.Types (Camera2D (..), Font, Rectangle (..), Texture, Vector2 (..))
import Raylib.Util (WindowResources, whileWindowOpen, whileWindowOpen0, whileWindowOpen_, withWindow)
import Raylib.Util.Colors (rayWhite)
import Startup (gameStartup)
import Tilemap (generateTilemap)
import UI (newLabel)
import Update (updateGame)
import Villager.Hauler (newHauler)

main :: IO ()
main = do
  withWindow
    screenWidth
    screenHeight
    "Protobuilder"
    75
    ( \window -> do
        initWorld' >>= runSystem (initializeGame window)
    )

initializeGame :: WindowResources -> System' ()
initializeGame window = do
  gameStartup window
  whileWindowOpen0 gameLoop
  liftIO $ closeWindow window

gameLoop :: System' ()
gameLoop = do
  handleInput

  (ActiveScene scene) <- get global
  updateGame scene

  liftIO $ do
    beginDrawing
    clearBackground rayWhite

  drawGame

  liftIO endDrawing
