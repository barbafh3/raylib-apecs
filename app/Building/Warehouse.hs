module Building.Warehouse where

import Apecs
import Components
  ( AtlasRegion (..),
    Collision (Collision),
    CollisionBox (CollisionBox),
    Position (..),
    ResourceStorage,
    Sprite (Sprite),
    StorageItem,
    StorageSpace (StorageSpace),
    System',
    TriggerCollision (..),
  )
import Data.HashMap (empty, fromList, insert)
import Raylib.Types (Rectangle (..), Vector2 (..))
import Tilemap (tileSizeF)

newWarehouse :: Vector2 -> Maybe ResourceStorage -> System' Entity
newWarehouse position@(Vector2 x y) mStartStorage = do
  warehouse <-
    newEntity
      ( Sprite,
        AtlasRegion (Rectangle (6.0 * tileSizeF) (4.0 * tileSizeF) tileSizeF tileSizeF),
        Position position,
        ( Collision False Nothing False,
          TriggerCollision,
          CollisionBox (Rectangle x y tileSizeF tileSizeF)
        )
      )

  case mStartStorage of
    Just storage -> set warehouse $ StorageSpace storage (fromList [] :: ResourceStorage)
    Nothing -> set warehouse $ StorageSpace empty empty

  return warehouse

addToStorageSpace :: Entity -> StorageItem -> System' ()
addToStorageSpace ety (resource, amount) = do
  (StorageSpace itemMap reservedMap) <- get ety
  set ety $ StorageSpace (insert resource amount itemMap) reservedMap

addItemToStorage :: [StorageItem] -> StorageItem -> [StorageItem]
addItemToStorage [] item = [item]
addItemToStorage ((iResource, iAmount) : items) item@(resource, amount)
  | resource == iResource = (iResource, iAmount + amount) : items
  | otherwise = addItemToStorage items item

removeItemToStorage :: [StorageItem] -> StorageItem -> [StorageItem]
removeItemToStorage [] item = []
removeItemToStorage ((iResource, iAmount) : items) item@(resource, amount)
  | resource == iResource = (iResource, iAmount - amount) : items
  | otherwise = removeItemToStorage items item
