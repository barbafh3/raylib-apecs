module Tilemap where
import Raylib.Types (Vector2(..), Rectangle(..))
import Components (System', Tile(..), Chunk(..), Tilemap (..))
import Foreign.C.Types (CFloat)
import Foreign.C
import GHC.Float (int2Float)
import Apecs
import System.Random (uniformR, initStdGen, newStdGen, randomRIO, randomIO)

tileSize, chunkTileSize, chunkRawSize :: Int
tileSize = 16
chunkTileSize = 16
chunkRawSize = tileSize * chunkTileSize

tileSizeCF, chunkRawSizeCF :: CFloat
tileSizeCF = CFloat $ int2Float tileSize
chunkRawSizeCF = CFloat $ int2Float chunkRawSize

generateTilemap :: System' ()
generateTilemap = do
    let chunkX = max 1 $ div 32 chunkTileSize
    let chunkY = max 1 $ div 32 chunkTileSize
    let coords = (,) <$> [0..chunkX - 1] <*> [0..chunkY - 1]
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
  tiles <- mapM createTile (permutate [worldStartX..worldEndX - 1] [worldStartY..worldEndY - 1])
  return $ Chunk rect tiles True

createTile :: (Int, Int) -> IO Tile
createTile (x, y) = do
  value <- randomRIO (0, 3 :: Int)
  let rx = CFloat $ int2Float (value * tileSize)
  let coord = Vector2 (CFloat $ int2Float x) (CFloat $ int2Float y)
  let rect = Rectangle rx 0.0 tileSizeCF tileSizeCF
  return $ Tile coord rect

permutate :: [a] -> [a] -> [(a, a)]
permutate l1 l2 = (,) <$> l1 <*> l2
