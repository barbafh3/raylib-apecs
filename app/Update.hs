module Update where

import Apecs
import Building.Warehouse (addItemToStorage)
import Collisions (detectCollisions)
import Components (CameraComponent (..), GlobalStorageList (..), InputList (..), KeyboardActionName (..), StorageSpace (..), System')
import Control.Monad (when)
import Data.HashMap
import qualified Data.HashMap as Map
import Input (isKeyboardActionDown, isKeyboardActionReleased, isMouseActionReleased)
import Raylib.Types (Camera2D (..), Vector2 (..))
import Tilemap (checkVisibleTilemapChunks)
import UI (updateUI)
import Utils (mergeMaps)
import Villagers (updateVillagerCollision, updateVillagerIdleState)

updateGame :: System' ()
updateGame = do
  checkVisibleTilemapChunks

  checkAddStone
  updateGlobalStorage

  updateVillagerIdleState
  updateVillagerCollision

  detectCollisions
  -- detectBodyCollisions
  -- detectTriggerCollisions

  moveCamera
  updateUI

updateGlobalStorage :: System' ()
updateGlobalStorage = do
  set global $ GlobalStorageList Map.empty
  (GlobalStorageList globalList) <- get global
  cmapM_ $ \(StorageSpace list _) -> set global $ GlobalStorageList $ mergeMaps list globalList

checkAddStone :: System' ()
checkAddStone = do
  actionReleased <- isKeyboardActionReleased AddStone
  cmapM_ $ \(StorageSpace list reserved, ety) -> do
    when actionReleased $ do set ety $ StorageSpace (Map.insertWith (+) "Stone" 10 list) reserved

moveCamera :: System' ()
moveCamera = do
  (CameraComponent speed (Camera2D offset (Vector2 x y) rotation zoom), cameraComponent) <- get global
  (InputList kbActions _) <- get global
  moveLeftActionDown <- isKeyboardActionDown MoveLeft
  moveRightActionDown <- isKeyboardActionDown MoveRight
  moveUpActionDown <- isKeyboardActionDown MoveUp
  moveDownActionDown <- isKeyboardActionDown MoveDown
  let newX
        | moveLeftActionDown = x - speed
        | moveRightActionDown = x + speed
        | otherwise = x
  let newY
        | moveUpActionDown = y - speed
        | moveDownActionDown = y + speed
        | otherwise = y
  set cameraComponent $ CameraComponent speed $ Camera2D offset (Vector2 newX newY) rotation zoom
