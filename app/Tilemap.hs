module Tilemap where

import Apecs
import Components (CameraComponent (..), Chunk (..), GameAtlasSets (..), System', Tile (..), Tilemap (..))
import Control.Monad (when)
import Data.Ratio ((%))
import Foreign.C
import Foreign.C.Types (CFloat)
import GHC.Float (int2Float)
import Raylib (drawTextureRec, getScreenHeight, getScreenWidth)
import Raylib.Colors (white)
import Raylib.Types (Camera2D (..), Rectangle (..), Texture, Vector2 (..))
import System.Random (initStdGen, newStdGen, randomIO, randomRIO, uniformR)
import Utils (areBoxesColliding, permutate)

tileSize, chunkTileSize, chunkRawSize :: Int
tileSize = 16
chunkTileSize = 16
chunkRawSize = tileSize * chunkTileSize

tileSizeCF, chunkRawSizeCF :: CFloat
tileSizeCF = CFloat $ int2Float tileSize
chunkRawSizeCF = CFloat $ int2Float chunkRawSize

-- tileSizeCI, chunkRawSizeCI :: CInt
-- tileSizeCI = int2 tileSize
-- chunkRawSizeCI = fi chunkRawSize

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
  let rect = Rectangle (CFloat $ int2Float worldStartX) (CFloat $ int2Float worldStartY) chunkRawSizeCF chunkRawSizeCF
  tiles <- mapM createTile (permutate [worldStartX .. worldEndX - 1] [worldStartY .. worldEndY - 1])
  return $ Chunk rect tiles False

createTile :: (Int, Int) -> IO Tile
createTile (x, y) = do
  -- This line is used to change the tile to a wierd one so the borders of the chunks are visible
  -- value <- if x /= 0 && y /= 0 && mod x 16 == 0 && mod y 16 == 0 then return 4 else randomRIO (0, 3 :: Int)
  value <- randomRIO (0, 3 :: Int)
  let rx = CFloat $ int2Float (value * tileSize)
  let coord = Vector2 (CFloat $ int2Float x) (CFloat $ int2Float y)
  let rect = Rectangle rx 0.0 tileSizeCF tileSizeCF
  return $ Tile coord rect

checkVisibleTilemapChunks :: System' ()
checkVisibleTilemapChunks = do
  (CameraComponent _ (Camera2D (Vector2 tx ty) _ _ zoom)) <- get global
  cmapM $ \(Tilemap chunks, tilemap) -> do
    screenWidth <- liftIO getScreenWidth
    screenHeight <- liftIO getScreenHeight
    let cw = CFloat $ int2Float screenWidth
    let ch = CFloat $ int2Float screenHeight
    let rect = Rectangle (-(tx / zoom)) (-(ty / zoom)) cw ch -- -> NOTE: I had to inverse and divide by the zoom to get the action camera position
    newChunks <- mapM (checkChunkVisibility rect) chunks
    set tilemap $ Tilemap newChunks

checkChunkVisibility :: Rectangle -> Chunk -> System' Chunk
checkChunkVisibility cameraRect (Chunk rect@(Rectangle rx ry rw rh) tiles visibility) = do
  let chunkRect = Rectangle (rx * tileSizeCF) (ry * tileSizeCF) chunkRawSizeCF chunkRawSizeCF
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
  let rect = Rectangle rx ry tileSizeCF tileSizeCF
  let vector = Vector2 (vx * tileSizeCF) (vy * tileSizeCF)
  drawTextureRec tileset rect vector white
  return ()
