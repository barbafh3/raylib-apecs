module Draw where
import Components (System', Tilemap (..), Chunk (..), Tile (..))
import Raylib
import Raylib.Types
import Raylib.Colors (white, lightGray, black)
import Apecs
import Tilemap (tileSizeCF, tileSize)

drawGame :: Texture -> Camera2D -> System' ()
drawGame tileset camera = do
    liftIO $ beginMode2D camera

    drawTilemap tileset
    drawSprites tileset

    liftIO endMode2D

    drawUI

drawTilemap :: Texture -> System' ()
drawTilemap tileset = cmapM_ $ \(Tilemap chunks) -> liftIO $ drawChunks tileset chunks

drawChunks :: Texture -> [Chunk] -> IO ()
drawChunks _ [] = return ()
drawChunks tileset (Chunk coord tiles visible : chunks)
    | visible = do 
        mapM_ (drawTile tileset) tiles
        drawChunks tileset chunks
    | otherwise = drawChunks tileset chunks

drawTile :: Texture -> Tile -> IO ()
drawTile tileset (Tile coord@(Vector2 vx vy) (Rectangle rx ry w h)) = do 
    let rect = Rectangle rx ry tileSizeCF tileSizeCF
    let vector = Vector2 (vx * tileSizeCF) (vy * tileSizeCF)
    drawTextureRec tileset rect vector white
    return ()

drawSprites :: Texture -> System' ()
drawSprites tileset = do
    return ()


drawUI :: System' ()
drawUI = do
    liftIO $ drawText "Basic raylib window" 30 40 20 black
    return ()

