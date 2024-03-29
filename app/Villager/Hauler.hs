{-# LANGUAGE ScopedTypeVariables #-}

module Villager.Hauler where

import Apecs
import Components
  ( AtlasRegion (..),
    BodyCollision (..),
    Collision (..),
    CollisionBox (..),
    CollisionType (..),
    IdleInfo (IdleInfo),
    Position (..),
    Sprite (..),
    System',
    TriggerCollision (..),
    Villager (..),
    VillagerState (..),
    VillagerType (..),
  )
import Control.Monad (when)
import Debug.Trace (trace)
import Foreign.C (CFloat (..))
import Linear (normalize)
import Raylib.Types (Rectangle (..), Vector2 (..))
import System.Random (randomRIO)
import Tilemap (tileSize, tileSizeF)
import Utils (normalizeVector, vectorLength, (|*#|), (|*|), (|+|), (|-|))
import Villagers (defaultIdleInfo)

newHauler :: Vector2 -> Vector2 -> CollisionType -> System' Entity
newHauler position@(Vector2 x y) idlePoint colType = do
  hauler <-
    newEntity
      ( Villager Hauler Idle,
        defaultIdleInfo idlePoint,
        Position position,
        Sprite,
        AtlasRegion (Rectangle (6.0 * tileSizeF) (12.0 * tileSizeF) tileSizeF tileSizeF),
        ( Collision False Nothing False,
          CollisionBox (Rectangle x y tileSizeF tileSizeF)
        )
      )
  case colType of
    Trigger -> set hauler TriggerCollision
    Body -> set hauler BodyCollision

  return hauler
