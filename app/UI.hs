{-# LANGUAGE BlockArguments #-}
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
  ( AtlasRegion (..),
    ButtonAction (..),
    ButtonState (..),
    CameraComponent (..),
    DrawBuildingPanel (DrawBuildingPanel),
    DrawResourcePanel (DrawResourcePanel),
    FontsComponent (..),
    GameAtlasSets (..),
    GlobalPlankLabel (..),
    GlobalStoneBrickLabel (..),
    GlobalStoneLabel (..),
    GlobalStorageLabel (..),
    GlobalStorageList (..),
    GlobalWoodLabel (..),
    InputState (..),
    Label (..),
    Layer (..),
    MouseActionName (LeftClick),
    Offset (..),
    Parent (..),
    Position (..),
    ResourcePanel (..),
    Scale (..),
    ShowFPS (..),
    StorageSpace (..),
    System',
    TextureButton (..),
    ToggleButton (ToggleButton),
    UIElement (..),
    UIImage (..),
    Visibility (..),
    World,
  )
import Constants (screenHeightF, screenWidthF)
import Control.Monad (forM_)
import Data.Foldable (find)
import qualified Data.HashMap as Map
import Data.Maybe (fromJust, fromMaybe)
import Foreign.C (CFloat (..))
import Foreign.C.Types (CInt)
import GHC.Base (when)
import GHC.Float (int2Float)
import Input (isMouseActionDown, isMouseActionReleased)
import Linear.Vector (Additive (zero))
import Raylib.Core (getMousePosition, getScreenHeight)
import Raylib.Core.Text (drawFPS, drawTextEx)
import Raylib.Core.Textures (drawTextureEx, drawTexturePro)
import Raylib.Types (Camera2D (..), Color, Rectangle (..), Texture, Vector2 (..))
import Raylib.Util.Colors (black, white)
import Tilemap (tileSizeF)
import Utils (isParentVisible, isPointInsideBox, mergeMaps, (|+|))

buttonActions :: [(ButtonAction, System' ())]
buttonActions =
  [ (ToggleFPSAction, toggleFPS),
    (ToggleDrawCollisionAction, toggleDrawCollision),
    (ToggleResourcePanel, toggleDrawResourcePanel),
    (ToggleBuildingPanel, toggleDrawBuildingPanel)
  ]

toggleDrawResourcePanel :: System' ()
toggleDrawResourcePanel = cmap $ \(ResourcePanel, Visibility visible) -> Visibility (not visible)

toggleDrawBuildingPanel :: System' ()
toggleDrawBuildingPanel = cmap $ \(DrawBuildingPanel enabled) -> DrawBuildingPanel (not enabled)

testMapUiStartup :: System' ()
testMapUiStartup = do
  (CameraComponent _ (Camera2D _ _ _ zoom)) <- get global

  set global $ DrawResourcePanel False
  set global $ DrawBuildingPanel False

  screenHeight <- liftIO getScreenHeight
  let screenHeightCF = CFloat $ int2Float screenHeight
  newToggleTextureButton (Vector2 10.0 $ screenHeightF - 10.0) 0 (Vector2 0.0 0.0) Nothing ToggleDrawCollisionAction
  newToggleTextureButton (Vector2 50.0 $ screenHeightF - 10.0) 0 (Vector2 3.0 0.0) Nothing ToggleFPSAction
  newToggleTextureButton (Vector2 90.0 $ screenHeightF - 10.0) 0 (Vector2 0.0 2.0) Nothing ToggleResourcePanel
  newToggleTextureButton (Vector2 130.0 $ screenHeightF - 10.0) 0 (Vector2 6.0 1.0) Nothing ToggleBuildingPanel

  resourcePanel <- newEntity (ResourcePanel, Visibility False)

  globalWoodIcon <-
    newEntity
      ( UIElement,
        Position (Vector2 10.0 10.0),
        Offset (Vector2 0.0 0.0),
        Scale 1.0,
        Layer 0,
        Visibility True,
        UIImage (Rectangle 0.0 (7.0 * tileSizeF) tileSizeF tileSizeF),
        Parent resourcePanel
      )
  globalWoodLabel <- newLabel "0" (Vector2 (15.0 + tileSizeF * zoom) 17.0) (Vector2 0.0 0.0) 0 20.0 1.0 black
  set globalWoodLabel (GlobalWoodLabel, Parent resourcePanel)

  globalStoneIcon <-
    newEntity
      ( UIElement,
        Position (Vector2 10.0 47.0),
        Offset (Vector2 0.0 0.0),
        Scale 1.0,
        Layer 0,
        Visibility True,
        UIImage (Rectangle (1.0 * tileSizeF) (7.0 * tileSizeF) tileSizeF tileSizeF),
        Parent resourcePanel
      )
  globalStoneLabel <- newLabel "0" (Vector2 (15.0 + tileSizeF * zoom) 54.0) (Vector2 0.0 0.0) 0 20.0 1.0 black
  set globalStoneLabel (GlobalStoneLabel, Parent resourcePanel)

  globaPlankIcon <-
    newEntity
      ( UIElement,
        Position (Vector2 10.0 84.0),
        Offset (Vector2 0.0 0.0),
        Scale 1.0,
        Layer 0,
        Visibility True,
        UIImage (Rectangle (2.0 * tileSizeF) (7.0 * tileSizeF) tileSizeF tileSizeF),
        Parent resourcePanel
      )
  globalPlankLabel <- newLabel "0" (Vector2 (15.0 + tileSizeF * zoom) 91.0) (Vector2 0.0 0.0) 0 20.0 1.0 black
  set globalPlankLabel (GlobalPlankLabel, Parent resourcePanel)

  globalStoneBrickIcon <-
    newEntity
      ( UIElement,
        Position (Vector2 10.0 121.0),
        Offset (Vector2 0.0 0.0),
        Scale 1.0,
        Layer 0,
        Visibility True,
        UIImage (Rectangle (3.0 * tileSizeF) (7.0 * tileSizeF) tileSizeF tileSizeF),
        Parent resourcePanel
      )
  globalStoneBrickLabel <- newLabel "0" (Vector2 (15.0 + tileSizeF * zoom) 128.0) (Vector2 0.0 0.0) 0 20.0 1.0 black
  set globalStoneBrickLabel (GlobalStoneBrickLabel, Parent resourcePanel)

  _ <-
    newEntity
      ( UIElement,
        Position (Vector2 0.0 0.0),
        Offset (Vector2 0.0 0.0),
        Scale 1.0,
        Layer 0,
        Visibility True
      )

  return ()

updateUI :: System' ()
updateUI = do
  checkButtonClick
  updateGlobalWoodLabel
  updateGlobalStoneLabel
  return ()

drawUI :: System' ()
drawUI = do
  drawUITextureButtons
  drawImages
  drawLabels

  cmapM_ $ \(ShowFPS enabled) -> when enabled $ liftIO $ drawFPS 10 10

drawUITextureButtons :: System' ()
drawUITextureButtons = do
  (GameAtlasSets _ uiAtlas) <- get global
  (CameraComponent _ (Camera2D _ _ _ zoom)) <- get global
  cmapM_ $
    \( TextureButton state _,
       AtlasRegion (Rectangle rx ry rw rh),
       UIElement,
       Position pos@(Vector2 ex ey),
       Scale scale,
       Visibility visible,
       ety
       ) -> do
        parentVisible <- isParentVisible ety
        when (visible && parentVisible) $ do
          let newRX = case state of
                Hovered -> rx + rw
                Held -> rx + rw * 2
                Toggled -> rx + rw * 2
                _ -> rx
          let src = Rectangle newRX ry rw rh
          let dest = Rectangle ex (ey - (rh * zoom)) (rw * scale * zoom) (rh * scale * zoom)
          liftIO $ drawTexturePro uiAtlas src dest (Vector2 0.0 0.0) 0.0 white

drawImages :: System' ()
drawImages = do
  (GameAtlasSets _ uiAtlas) <- get global
  (CameraComponent _ (Camera2D _ _ _ zoom)) <- get global
  cmapM_ $
    \(UIElement, Position pos@(Vector2 ex ey), Scale scale, Visibility visible, UIImage rect, ety) -> do
      parentVisible <- isParentVisible ety
      when (visible && parentVisible) $ do
        let src = rect
        let dest = Rectangle ex ey (tileSizeF * scale * zoom) (tileSizeF * scale * zoom)
        liftIO $ drawTexturePro uiAtlas src dest (Vector2 0.0 0.0) 0.0 white

-- cmapM_ $
--   \(ResourcePanel, Label text fontSize spacing color, UIElement pos offset scale layer visible) -> do
--     liftIO $ drawTextEx font text (pos |+| offset) fontSize spacing color

drawLabels :: System' ()
drawLabels = do
  (FontsComponent font) <- get global
  cmapM_ $
    \( Label text fontSize spacing color,
       UIElement,
       Position pos,
       Offset offset,
       Scale scale,
       Visibility visible,
       ety
       ) -> do
        parentVisible <- isParentVisible ety
        when (visible && parentVisible) $
          liftIO $
            drawTextEx font text (pos |+| offset) fontSize spacing color

newLabel :: String -> Vector2 -> Vector2 -> CInt -> Float -> Float -> Color -> System' Entity
newLabel text pos offset layer fontSize spacing color = do
  let label = Label text fontSize spacing color
  newEntity (UIElement, Position pos, Offset offset, Scale 1.0, Layer layer, Visibility True, label)

newTextureButton :: Vector2 -> CInt -> Vector2 -> Maybe Vector2 -> ButtonAction -> System' ()
newTextureButton pos layer (Vector2 vx vy) (Just (Vector2 sx sy)) action = do
  let region = AtlasRegion (Rectangle (vx * tileSizeF) (vy * tileSizeF) sx sy)
  newEntity
    ( UIElement,
      Position pos,
      Offset (Vector2 0.0 0.0),
      Scale 1.0,
      Layer layer,
      Visibility True,
      TextureButton Normal action,
      region
    )
  return ()
newTextureButton pos layer (Vector2 vx vy) Nothing action = do
  let region = AtlasRegion (Rectangle (vx * tileSizeF) (vy * tileSizeF) tileSizeF tileSizeF)
  newEntity
    ( UIElement,
      Position pos,
      Offset (Vector2 0.0 0.0),
      Scale 1.0,
      Layer layer,
      Visibility True,
      TextureButton Normal action,
      region
    )
  return ()

newToggleTextureButton :: Vector2 -> CInt -> Vector2 -> Maybe Vector2 -> ButtonAction -> System' ()
newToggleTextureButton pos layer (Vector2 vx vy) Nothing action = do
  let region = AtlasRegion (Rectangle (vx * tileSizeF) (vy * tileSizeF) tileSizeF tileSizeF)
  newEntity
    ( UIElement,
      Position pos,
      Offset (Vector2 0.0 0.0),
      Scale 1.0,
      Layer layer,
      Visibility True,
      ( TextureButton Normal action,
        region,
        ToggleButton
      )
    )
  return ()
newToggleTextureButton pos layer (Vector2 vx vy) (Just (Vector2 sx sy)) action = do
  let region = AtlasRegion (Rectangle (vx * tileSizeF) (vy * tileSizeF) sx sy)
  newEntity
    ( UIElement,
      Position pos,
      Offset (Vector2 0.0 0.0),
      Scale 1.0,
      Layer layer,
      Visibility True,
      ( TextureButton Normal action,
        region,
        ToggleButton
      )
    )
  return ()

getButtonAction :: ButtonAction -> Maybe (ButtonAction, System' ())
getButtonAction action = find (\(act, funct) -> act == action) buttonActions

updateGlobalWoodLabel :: System' ()
updateGlobalWoodLabel = do
  (GlobalStorageList list) <- get global
  cmapM_ $ \(GlobalWoodLabel, Label text fontSize spacing color, UIElement, ety) -> do
    let woodCount = fromMaybe 0 $ Map.lookup "Wood" list
    set ety (Label (show woodCount) fontSize spacing color)

updateGlobalStoneLabel :: System' ()
updateGlobalStoneLabel = do
  (GlobalStorageList list) <- get global
  cmapM_ $ \(GlobalStoneLabel, Label text fontSize spacing color, UIElement, ety) -> do
    let stoneCount = fromMaybe 0 $ Map.lookup "Stone" list
    set ety (Label (show stoneCount) fontSize spacing color)

updateGlobalPlankLabel :: System' ()
updateGlobalPlankLabel = do
  (GlobalStorageList list) <- get global
  cmapM_ $ \(GlobalPlankLabel, Label text fontSize spacing color, UIElement, ety) -> do
    let stoneCount = fromMaybe 0 $ Map.lookup "Plank" list
    set ety (Label (show stoneCount) fontSize spacing color)

updateGlobalStoneBrickLabel :: System' ()
updateGlobalStoneBrickLabel = do
  (GlobalStorageList list) <- get global
  cmapM_ $ \(GlobalStoneBrickLabel, Label text fontSize spacing color, UIElement, ety) -> do
    let stoneCount = fromMaybe 0 $ Map.lookup "StoneBrick" list
    set ety (Label (show stoneCount) fontSize spacing color)

checkButtonClick :: System' ()
checkButtonClick = do
  (CameraComponent _ (Camera2D _ _ _ zoom)) <- get global
  cmapM_ $ \(TextureButton state action, AtlasRegion (Rectangle rx ry rw rh), UIElement, Position (Vector2 ex ey), Not :: Not ToggleButton, ety) -> do
    mousePos <- liftIO getMousePosition
    let buttonBox = Rectangle ex (ey - (rh * zoom)) (rw * zoom) (rh * zoom)
    leftMouseDown <- isMouseActionDown LeftClick
    leftMouseReleased <- isMouseActionReleased LeftClick
    if isPointInsideBox mousePos buttonBox
      then do
        set ety $ TextureButton Hovered action
        when leftMouseDown $ set ety $ TextureButton Held action
        when leftMouseReleased $ forM_ (getButtonAction action) snd
      else set ety $ TextureButton Normal action

  cmapM_ $ \(TextureButton state action, AtlasRegion (Rectangle rx ry rw rh), UIElement, Position (Vector2 ex ey), ToggleButton, ety) -> do
    mousePos <- liftIO getMousePosition
    let buttonBox = Rectangle ex (ey - (rh * zoom)) (rw * zoom) (rh * zoom)
    leftMouseReleased <- isMouseActionReleased LeftClick
    if isPointInsideBox mousePos buttonBox
      then do
        if leftMouseReleased
          then do
            if state == Toggled
              then set ety $ TextureButton Hovered action
              else set ety $ TextureButton Toggled action
            forM_ (getButtonAction action) snd
          else when (state /= Toggled) $ set ety $ TextureButton Hovered action
      else when (state /= Toggled) $ set ety $ TextureButton Normal action

toggleFPS :: System' ()
toggleFPS = cmap $ \(ShowFPS showFPS) -> ShowFPS (not showFPS)
