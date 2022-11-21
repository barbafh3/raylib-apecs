{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module UI where

import Apecs
import Building.Warehouse (addItemToStorage)
import Collisions (toggleDrawCollision)
import Components
  ( ButtonAction (..),
    ButtonState (..),
    CameraComponent (..),
    FontsComponent (..),
    GameAtlasSets (..),
    GlobalStorageLabel (..),
    GlobalStorageList (..),
    InputState (..),
    Label (..),
    MouseActionName (LeftClick),
    ShowFPS (..),
    StorageSpace (..),
    System',
    TextureButton (..),
    ToggleButton (ToggleButton),
    UIElement (..),
    World,
  )
import Control.Monad (forM_)
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Foreign.C (CFloat (..))
import GHC.Base (when)
import GHC.Float (int2Float)
import Input (isMouseActionDown, isMouseActionReleased)
import Linear.Vector (Additive (zero))
import Raylib
  ( drawCircle,
    drawFPS,
    drawText,
    drawTexturePro,
    getMousePosition,
    getScreenHeight,
    isMouseButtonDown,
    isMouseButtonReleased,
    measureText,
  )
import Raylib.Colors (black, white)
import Raylib.Constants (mouseButton'left)
import Raylib.Types (Camera2D (..), Color, Rectangle (..), Texture, Vector2 (..))
import Tilemap (tileSizeCF)
import Utils (isPointInsideBox, mergeMaps, (|+|))

buttonActions :: [(ButtonAction, System' ())]
buttonActions =
  [ (ToggleFPSAction, toggleFPS),
    (ToggleDrawCollisionAction, toggleDrawCollision)
  ]

uiStartup :: System' ()
uiStartup = do
  screenHeight <- liftIO getScreenHeight
  let screenHeightCF = CFloat $ int2Float screenHeight
  newToggleTextureButton (Vector2 10.0 $ screenHeightCF - 10.0) 0 (Vector2 0.0 0.0) ToggleDrawCollisionAction
  newToggleTextureButton (Vector2 50.0 $ screenHeightCF - 10.0) 0 (Vector2 3.0 0.0) ToggleFPSAction
  return ()

updateUI :: System' ()
updateUI = do
  checkButtonClick
  updateGlobalStorageLabel
  return ()

drawUI :: System' ()
drawUI = do
  cmapM_ $ \(ShowFPS enabled) -> when enabled $ liftIO $ drawFPS 10 10
  drawUITextureButtons
  drawLabels
  return ()

drawUITextureButtons :: System' ()
drawUITextureButtons = do
  (GameAtlasSets _ uiAtlas) <- get global
  (CameraComponent _ (Camera2D _ _ _ zoom)) <- get global
  cmapM_ $ \(TextureButton (Rectangle rx ry rw rh) state _, UIElement pos@(Vector2 ex ey) _ _ visible) -> do
    when visible $ do
      let newRX = case state of
            Hovered -> rx + 16.0
            Held -> rx + 32.0
            Toggled -> rx + 32.0
            _ -> rx
      let src = Rectangle newRX ry rw rh
      let dest = Rectangle ex (ey - (tileSizeCF * zoom)) (tileSizeCF * zoom) (tileSizeCF * zoom)
      liftIO $ drawTexturePro uiAtlas src dest (Vector2 0.0 0.0) 0.0 white

drawLabels :: System' ()
drawLabels = do
  (FontsComponent font) <- get global
  cmapM_ $ \(Label text fontSize spacing color, UIElement pos offset layer visible) ->
    -- when visible $ liftIO $ drawTextEx font text (pos |+| offset) fontSize spacing color
    when visible $ do
      let (Vector2 x y) = pos |+| offset
      -- liftIO $ print $ "Pos: " ++ show pos ++ " - Offset: " ++ show offset
      liftIO $ drawText text (round x) (round y) (round fontSize) color

newLabel :: String -> Vector2 -> Vector2 -> Int -> Float -> Float -> Color -> System' Entity
newLabel text pos offset layer fontSize spacing color = do
  let uiElement = UIElement pos offset layer True
  let label = Label text fontSize spacing color
  newEntity (uiElement, label)

newTextureButton :: Vector2 -> Int -> Vector2 -> ButtonAction -> System' ()
newTextureButton pos layer (Vector2 vx vy) action = do
  let uiElement = UIElement pos (Vector2 0.0 0.0) layer True
  let button = TextureButton (Rectangle (vx * tileSizeCF) (vy * tileSizeCF) tileSizeCF tileSizeCF) Normal action
  newEntity (uiElement, button)
  return ()

newToggleTextureButton :: Vector2 -> Int -> Vector2 -> ButtonAction -> System' ()
newToggleTextureButton pos layer (Vector2 vx vy) action = do
  let uiElement = UIElement pos (Vector2 0.0 0.0) layer True
  let button = TextureButton (Rectangle (vx * tileSizeCF) (vy * tileSizeCF) tileSizeCF tileSizeCF) Normal action
  newEntity (uiElement, button, ToggleButton)
  return ()

getButtonAction :: ButtonAction -> Maybe (ButtonAction, System' ())
getButtonAction action = find (\(act, funct) -> act == action) buttonActions

updateGlobalStorageLabel :: System' ()
updateGlobalStorageLabel = do
  (GlobalStorageList list) <- get global
  cmapM_ $ \(GlobalStorageLabel, Label text fontSize spacing color, UIElement pos (Vector2 ox oy) layer visible, ety) -> do
    textSize <- liftIO $ measureText (show list) (round fontSize)
    let newX = CFloat (int2Float $ div textSize 2)
    set ety (UIElement pos (Vector2 (-newX) oy) layer visible, Label (show list) fontSize spacing color)

checkButtonClick :: System' ()
checkButtonClick = do
  (CameraComponent _ (Camera2D _ _ _ zoom)) <- get global
  cmapM_ $ \(TextureButton rect state action, UIElement (Vector2 ex ey) _ _ _, Not :: Not ToggleButton, ety) -> do
    mousePos <- liftIO getMousePosition
    let buttonBox = Rectangle ex (ey - (tileSizeCF * zoom)) (tileSizeCF * zoom) (tileSizeCF * zoom)
    leftMouseDown <- isMouseActionDown LeftClick
    leftMouseReleased <- isMouseActionReleased LeftClick
    if isPointInsideBox mousePos buttonBox
      then do
        set ety $ TextureButton rect Hovered action
        when leftMouseDown $ set ety $ TextureButton rect Held action
        when leftMouseReleased $ forM_ (getButtonAction action) snd
      else set ety $ TextureButton rect Normal action

  cmapM_ $ \(TextureButton rect state action, UIElement (Vector2 ex ey) _ _ _, ToggleButton, ety) -> do
    mousePos <- liftIO getMousePosition
    let buttonBox = Rectangle ex (ey - (tileSizeCF * zoom)) (tileSizeCF * zoom) (tileSizeCF * zoom)
    leftMouseReleased <- isMouseActionReleased LeftClick
    if isPointInsideBox mousePos buttonBox
      then do
        if leftMouseReleased
          then do
            if state == Toggled
              then set ety $ TextureButton rect Hovered action
              else set ety $ TextureButton rect Toggled action
            forM_ (getButtonAction action) snd
          else when (state /= Toggled) $ set ety $ TextureButton rect Hovered action
      else when (state /= Toggled) $ set ety $ TextureButton rect Normal action

toggleFPS :: System' ()
toggleFPS = cmap $ \(ShowFPS showFPS) -> ShowFPS (not showFPS)
