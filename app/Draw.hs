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
import Raylib.Core (beginMode2D, endMode2D)
import Raylib.Core.Textures (drawTextureRec)
import Raylib.Types
import Raylib.Util.Colors (white)
import Tilemap (drawTilemap, tileSize)
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
