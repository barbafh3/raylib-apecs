{-# LANGUAGE ScopedTypeVariables #-}

module Hauler where

import Apecs
import Components
  ( CollisionBox (..),
    IdleInfo (IdleInfo),
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
import Raylib (getFrameTime)
import Raylib.Types (Rectangle (..), Vector2 (..))
import System.Random (randomRIO)
import Tilemap (tileSize, tileSizeCF)
import Utils (normalizeVector, vectorLength, (|*#|), (|*|), (|+|), (|-|))

defaultIdleInfo :: Vector2 -> IdleInfo
defaultIdleInfo position = IdleInfo position 0.0 (3.0, 5.0) 10.0 (Vector2 0.0 0.0)

newHauler :: Vector2 -> Vector2 -> System' Entity
newHauler position@(Vector2 x y) idlePoint = do
  newEntity
    ( Villager Hauler Idle,
      defaultIdleInfo position,
      Sprite position (Rectangle (6.0 * tileSizeCF) (12.0 * tileSizeCF) tileSizeCF tileSizeCF),
      ( TriggerCollision False Nothing False,
        CollisionBox (Rectangle x y tileSizeCF tileSizeCF)
      )
    )

updateVillagerCollision :: System' ()
updateVillagerCollision =
  cmapM_ $ \(Sprite (Vector2 x y) _, CollisionBox (Rectangle rx ry rw rh), ety) -> do
    set ety $ CollisionBox (Rectangle x y rw rh)

updateVillagerIdleState :: System' ()
updateVillagerIdleState =
  cmapM_ $ \(idleInfo@(IdleInfo idlePos timer range radius target), sprite@(Sprite pos rect), ety :: Entity) -> do
    delta <- liftIO $ CFloat <$> getFrameTime
    let newTimer = updateIdleTick timer delta
    set ety (IdleInfo idlePos newTimer range radius target)
    when (newTimer <= 0.0) $ do
      newIdleInfo <- liftIO $ getNewTarget idleInfo
      set ety newIdleInfo
    let hasNotArrived = vectorLength (target |-| pos) > 1.0
    when (vectorLength (target |-| pos) > 1.0) $ do
      set ety (Sprite (moveVillager target pos delta) rect)

updateIdleTick :: CFloat -> CFloat -> CFloat
updateIdleTick timer delta =
  if timer > 0.0
    then timer - delta
    else timer

getNewTarget :: IdleInfo -> IO IdleInfo
getNewTarget (IdleInfo pos@(Vector2 x y) _ range@(s, e) radius target) = do
  randX <- randomRIO (x - radius, x + radius)
  randY <- randomRIO (y - radius, y + radius)
  newTimer <- randomRIO (s, e)
  return $ IdleInfo pos newTimer range radius (Vector2 randX randY)

moveVillager :: Vector2 -> Vector2 -> CFloat -> Vector2
moveVillager target pos delta = newPos
  where
    direction = target |-| pos
    normalizedDirection = normalizeVector direction
    newPos = pos |+| (normalizedDirection |*#| (50.0 * delta))