module Hauler where

import Apecs
import Components (CollisionBox (..), Hauler (..), Sprite (..), System', TriggerCollision (..))
import Raylib.Types (Rectangle (..), Vector2 (..))
import Tilemap (tileSize, tileSizeCF)

newHauler :: Vector2 -> Vector2 -> System' Entity
newHauler position@(Vector2 x y) idlePoint = do
  newEntity
    ( Hauler,
      Sprite position (Rectangle (6.0 * tileSizeCF) (12.0 * tileSizeCF) tileSizeCF tileSizeCF),
      TriggerCollision False Nothing,
      CollisionBox (Rectangle x y tileSizeCF tileSizeCF)
    )