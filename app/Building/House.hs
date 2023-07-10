module Building.House where

import Apecs
import Components
  ( AtlasRegion (..),
    Collision (..),
    CollisionBox (..),
    Position (..),
    Sprite (..),
    System',
    TriggerCollision (..),
  )
import Raylib.Types (Rectangle (..), Vector2 (..))
import Tilemap (tileSizeF)

newHouse :: Vector2 -> System' Entity
newHouse position@(Vector2 x y) =
  newEntity
    ( Sprite,
      AtlasRegion (Rectangle (2.0 * tileSizeF) (1.0 * tileSizeF) tileSizeF tileSizeF),
      Position position,
      ( Collision False Nothing False,
        TriggerCollision,
        CollisionBox (Rectangle x y tileSizeF tileSizeF)
      )
    )
