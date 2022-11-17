module Update where

import Apecs
import Collision (detectBodyCollisions, detectTriggerCollisions)
import Components (CameraComponent (..), InputList (..), KeyboardActionName (..), System')
import Hauler (updateVillagerCollision, updateVillagerIdleState)
import Input (isKeyboardActionDown)
import Raylib.Types (Camera2D (..), Vector2 (..))
import Tilemap (checkVisibleTilemapChunks)
import UI (updateUI)

updateGame :: System' ()
updateGame = do
  checkVisibleTilemapChunks

  updateVillagerIdleState
  updateVillagerCollision

  detectBodyCollisions
  detectTriggerCollisions

  moveCamera
  updateUI

moveCamera :: System' ()
moveCamera = do
  (CameraComponent speed (Camera2D (Vector2 x y) offset rotation zoom), cameraComponent) <- get global
  (InputList kbActions _) <- get global
  moveLeftActionDown <- isKeyboardActionDown MoveLeft
  moveRightActionDown <- isKeyboardActionDown MoveRight
  moveUpActionDown <- isKeyboardActionDown MoveUp
  moveDownActionDown <- isKeyboardActionDown MoveDown
  let newX
        | moveLeftActionDown = x + speed
        | moveRightActionDown = x - speed
        | otherwise = x
  let newY
        | moveUpActionDown = y + speed
        | moveDownActionDown = y - speed
        | otherwise = y
  -- >> NOTE: Had to invert the values cuz the camera is inverted for some reason
  set cameraComponent $ CameraComponent speed $ Camera2D (Vector2 newX newY) offset rotation zoom
