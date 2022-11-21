module Draw where

import Apecs
import Collisions (drawCollisions)
import Components (CameraComponent (..), Chunk (..), GameAtlasSets (GameAtlasSets), Sprite (..), System', Tile (..), Tilemap (..))
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
drawSprites = cmapM_ $ \(Sprite position rect) -> do
  (GameAtlasSets tileset uiAtlas) <- get global
  liftIO $ drawTextureRec tileset rect position white
