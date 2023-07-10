module Tilemap where

import Apecs
import Components (CameraComponent (..), Chunk (..), GameAtlasSets (..), System', Tile (..), Tilemap (..))
import Control.Monad (when)
import Data.Ratio ((%))
import Foreign.C
import Foreign.C.Types (CFloat)
import GHC.Float (int2Float)
import Raylib.Core (getScreenHeight, getScreenWidth)
import Raylib.Core.Textures (drawTextureRec)
import Raylib.Types (Camera2D (..), Rectangle (..), Texture, Vector2 (..))
import Raylib.Util.Colors (white)
import System.Random (initStdGen, newStdGen, randomIO, randomRIO, uniformR)
import Utils (areBoxesColliding, permutate)

tileSize, chunkTileSize, chunkRawSize :: Int
tileSize = 16
chunkTileSize = 16
chunkRawSize = tileSize * chunkTileSize

tileSizeF :: Float
tileSizeF = int2Float tileSize

chunkTileSizeF = int2Float chunkTileSize

chunkRawSizeF = int2Float chunkRawSize

generateTilemap :: Int -> Int -> System' ()
generateTilemap width height = do
  let chunkX = max 1 $ div width chunkTileSize
  let chunkY = max 1 $ div height chunkTileSize
  let coords = (,) <$> [0 .. chunkX - 1] <*> [0 .. chunkY - 1]
  chunks <- liftIO $ mapM generateChunk coords
  _ <- newEntity $ Tilemap chunks
  return ()

generateChunk :: (Int, Int) -> IO Chunk
generateChunk coord@(originX, originY) = do
  let worldStartX = originX * chunkTileSize
  let worldStartY = originY * chunkTileSize
  let worldEndX = worldStartX + chunkTileSize
  let worldEndY = worldStartY + chunkTileSize
  let rect = Rectangle (int2Float worldStartX) (int2Float worldStartY) chunkRawSizeF chunkRawSizeF
  tiles <- mapM createTile (permutate [worldStartX .. worldEndX - 1] [worldStartY .. worldEndY - 1])
  return $ Chunk rect tiles False

createTile :: (Int, Int) -> IO Tile
createTile (x, y) = do
  value <- randomRIO (0, 3 :: Int)
  let rx = int2Float (value * tileSize)
  let coord = Vector2 (int2Float x) (int2Float y)
  let rect = Rectangle rx 0.0 tileSizeF tileSizeF
  return $ Tile coord rect

checkVisibleTilemapChunks :: System' ()
checkVisibleTilemapChunks = do
  (CameraComponent _ (Camera2D _ (Vector2 tx ty) _ zoom)) <- get global
  cmapM $ \(Tilemap chunks, tilemap) -> do
    screenWidth <- liftIO getScreenWidth
    screenHeight <- liftIO getScreenHeight
    let cw = int2Float screenWidth
    let ch = int2Float screenHeight
    let rect = Rectangle (tx - 16) (ty - 16) cw ch
    newChunks <- mapM (checkChunkVisibility rect) chunks
    set tilemap $ Tilemap newChunks

checkChunkVisibility :: Rectangle -> Chunk -> System' Chunk
checkChunkVisibility cameraRect (Chunk rect@(Rectangle rx ry rw rh) tiles visibility) = do
  let chunkRect = Rectangle (rx * tileSizeF) (ry * tileSizeF) chunkRawSizeF chunkRawSizeF
  return $ Chunk rect tiles $ areBoxesColliding chunkRect cameraRect

drawTilemap :: System' ()
drawTilemap = do
  (GameAtlasSets tileset _) <- get global
  cmapM_ $ \(Tilemap chunks) -> liftIO $ drawChunks tileset chunks

drawChunks :: Texture -> [Chunk] -> IO ()
drawChunks _ [] = return ()
drawChunks tileset (Chunk (Rectangle rx ry rw rh) tiles visible : chunks)
  | visible = do
      mapM_ (drawTile tileset) tiles
      drawChunks tileset chunks
  | otherwise = drawChunks tileset chunks

drawTile :: Texture -> Tile -> IO ()
drawTile tileset (Tile coord@(Vector2 vx vy) (Rectangle rx ry w h)) = do
  let rect = Rectangle rx ry tileSizeF tileSizeF
  let vector = Vector2 (vx * tileSizeF) (vy * tileSizeF)
  drawTextureRec tileset rect vector white
  return ()
