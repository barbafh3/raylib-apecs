module Update where

import Components (System', CameraComponent (..), InputList (..))
import Tilemap (checkVisibleTilemapChunks)
import Input (isActionDown)
import Raylib.Types (Camera2D (..), Vector2 (..))
import Apecs 
import UI (updateUI)

updateGame :: System' ()
updateGame = do
    checkVisibleTilemapChunks
    moveCamera
    updateUI

moveCamera :: System' ()
moveCamera = 
  cmapM_ $ \(CameraComponent speed (Camera2D (Vector2 x y) offset rotation zoom), cameraComponent) ->
    cmapM_ $ \(InputList actions) -> do
      let newX 
            | isActionDown "MoveLeft" actions = x + speed 
            | isActionDown "MoveRight" actions = x - speed 
            | otherwise = x
      let newY 
            | isActionDown "MoveUp" actions = y + speed 
            | isActionDown "MoveDown" actions = y - speed 
            | otherwise = y
      -- >> NOTE: Had to invert the values cuz the camera is inverted for some reason
      set cameraComponent $ CameraComponent speed $ Camera2D (Vector2 newX newY) offset rotation zoom
