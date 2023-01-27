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
import Tilemap (tileSizeCF)

newHouse :: Vector2 -> System' Entity
newHouse position@(Vector2 x y) =
  newEntity
    ( Sprite,
      AtlasRegion (Rectangle (2.0 * tileSizeCF) (1.0 * tileSizeCF) tileSizeCF tileSizeCF),
      Position position,
      ( Collision False Nothing False,
        TriggerCollision,
        CollisionBox (Rectangle x y tileSizeCF tileSizeCF)
      )
    )
