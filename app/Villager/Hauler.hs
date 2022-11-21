{-# LANGUAGE ScopedTypeVariables #-}

module Villager.Hauler where

import Apecs
import Components
  ( CollisionBox (..),
    IdleInfo (IdleInfo),
    Sprite (..),
    System',
    Villager (..),
    VillagerState (..),
    VillagerType (..), CollisionType (..), Collision (..),
  )
import Control.Monad (when)
import Debug.Trace (trace)
import Foreign.C (CFloat (..))
import Linear (normalize)
import Raylib (getFrameTime)
import Raylib.Types (Rectangle (..), Vector2 (..))
import System.Random (randomRIO)
import Tilemap (tileSize, tileSizeCF)
import Utils (normalizeVector, vectorLength, (|*#|), (|*|), (|+|), (|-|))
import Villagers (defaultIdleInfo)

newHauler :: Vector2 -> Vector2 -> CollisionType -> System' Entity
newHauler position@(Vector2 x y) idlePoint colType = do
  newEntity
    ( Villager Hauler Idle,
      defaultIdleInfo idlePoint,
      Sprite position (Rectangle (6.0 * tileSizeCF) (12.0 * tileSizeCF) tileSizeCF tileSizeCF),
      ( Collision colType False Nothing False,
        CollisionBox (Rectangle x y tileSizeCF tileSizeCF)
      )
    )
