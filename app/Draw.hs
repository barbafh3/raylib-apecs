module Draw where

import Apecs
import Collisions (drawCollisions)
import Components
  ( AtlasRegion (..),
    CameraComponent (..),
    Chunk (..),
    GameAtlasSets (GameAtlasSets),
    Position (..),
    Sprite (..),
    System',
    Tile (..),
    Tilemap (..),
  )
import Raylib
import Raylib.Colors (black, lightGray, white)
import Raylib.Types
import Tilemap (drawTilemap, tileSize, tileSizeCF)
import UI (drawUI)

drawGame :: System' ()
drawGame = do
  (CameraComponent _ camera) <- get global

  liftIO $ beginMode2D camera
  drawTilemap
  drawSprites
  drawCollisions

  liftIO endMode2D

  drawUI

drawSprites :: System' ()
drawSprites = cmapM_ $ \(Sprite, AtlasRegion rect, Position position) -> do
  (GameAtlasSets tileset _) <- get global
  liftIO $ drawTextureRec tileset rect position white
