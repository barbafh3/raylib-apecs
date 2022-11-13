module Draw where

import Components (System', Tilemap (..), Chunk (..), Tile (..), CameraComponent (..))
import Raylib
import Raylib.Types
import Raylib.Colors (white, lightGray, black)
import Apecs
import Tilemap (tileSizeCF, tileSize, drawTilemap)
import UI (drawUI)

drawGame :: Texture -> Texture -> System' ()
drawGame tileset uiAtlas = 
  cmapM $ \(CameraComponent _ camera) -> do
    liftIO $ beginMode2D camera

    drawTilemap tileset

    liftIO endMode2D

    drawUI uiAtlas

