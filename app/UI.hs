{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module UI where

import Apecs
import Components (
  ButtonState (..), 
  CameraComponent (..), 
  InputState (..), 
  ShowFPS (..), 
  System', 
  TextureButton (..), 
  UIElement (..), 
  World, 
  ButtonAction (..)
  )
import Foreign.C (CFloat (..))
import GHC.Float (int2Float)
import Linear.Vector (Additive (zero))
import Raylib (drawFPS, drawTexturePro, getMousePosition, getScreenHeight, isMouseButtonDown, isMouseButtonReleased)
import Raylib.Colors (white)
import Raylib.Constants (mouseButton'left)
import Raylib.Types (Camera2D (..), Rectangle (..), Texture, Vector2 (..))
import Tilemap (tileSizeCF)
import Utils (isPointInsideBox)
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Control.Monad (forM_)
import GHC.Base (when)
import Collision (toggleDrawCollision)


buttonActions :: [(ButtonAction, System' ())]
buttonActions = [
  (ToggleFPSAction, toggleFPS),
  (ToggleDrawCollisionAction, toggleDrawCollision)
  ]

uiStartup :: System' ()
uiStartup = do
  screenHeight <- liftIO getScreenHeight
  let screenHeightCF = CFloat $ int2Float screenHeight
  newTextureButton (Vector2 10.0 $ screenHeightCF - 10.0) (Vector2 0.0 0.0) 0 (Vector2 0.0 0.0) tileSizeCF ToggleDrawCollisionAction
  newTextureButton (Vector2 50.0 $ screenHeightCF - 10.0) (Vector2 0.0 0.0) 0 (Vector2 3.0 0.0) tileSizeCF ToggleFPSAction
  return ()

updateUI :: System' ()
updateUI = do
  checkButtonClick
  return ()

drawUI :: Texture -> System' ()
drawUI uiAtlas = do
  cmapM_ $ \(ShowFPS enabled) -> when enabled $ liftIO $ drawFPS 10 10
  drawUITextureButtons uiAtlas
  return ()

drawUITextureButtons :: Texture -> System' ()
drawUITextureButtons uiAtlas = do
  cmapM_ $ \(CameraComponent _ (Camera2D _ _ _ zoom)) -> do
    cmapM_ $ \(TextureButton (Rectangle rx ry rw rh) state _, UIElement pos@(Vector2 ex ey) _ _ visible) -> do
      when visible $ do
        let newRX = case state of
              Hovered -> rx + 16.0
              Held -> rx + 32.0
              _ -> rx
        let src = Rectangle newRX ry rw rh
        let dest = Rectangle ex (ey - (tileSizeCF * zoom)) (tileSizeCF * zoom) (tileSizeCF * zoom)
        liftIO $ drawTexturePro uiAtlas src dest (Vector2 0.0 0.0) 0.0 white

newTextureButton :: Vector2 -> Vector2 -> Int -> Vector2 -> CFloat -> ButtonAction -> System' ()
newTextureButton pos offset layer (Vector2 vx vy) tileSize action = do
  let uiElement = UIElement pos offset layer True
  let button = TextureButton (Rectangle (vx * tileSize) (vy * tileSize) tileSize tileSize) Normal action
  newEntity (uiElement, button)
  return ()

getButtonAction :: ButtonAction -> Maybe (ButtonAction, System' ())
getButtonAction action = find (\(act, funct) -> act == action) buttonActions

checkButtonClick :: System' ()
checkButtonClick =
  cmapM_ $ \(CameraComponent speed (Camera2D _ _ _ zoom)) -> do
    cmapM_ $ \(TextureButton rect state action, UIElement (Vector2 ex ey) _ _ _, ety) -> do
      mousePos <- liftIO getMousePosition
      let buttonBox = Rectangle ex (ey - (tileSizeCF * zoom)) (tileSizeCF * zoom) (tileSizeCF * zoom)
      leftMouseDown <- liftIO $ isMouseButtonDown mouseButton'left
      leftMouseReleased <- liftIO $ isMouseButtonReleased mouseButton'left
      let insideButton = isPointInsideBox mousePos buttonBox
      when (insideButton && leftMouseReleased) (forM_ (getButtonAction action) snd)
      let newState 
            | insideButton && leftMouseDown && not leftMouseReleased = Held
            | insideButton && not leftMouseDown && not leftMouseReleased = Hovered
            | otherwise = Normal
      set ety $ TextureButton rect newState action

toggleFPS :: System' ()
toggleFPS = cmapM_ $ \(ShowFPS showFPS, ety) -> set ety $ ShowFPS (not showFPS)
