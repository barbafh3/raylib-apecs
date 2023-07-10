{-# LANGUAGE ScopedTypeVariables #-}

module Villagers where

import Apecs
import Components
  ( CollisionBox (..),
    IdleInfo (..),
    Position (..),
    Sprite (..),
    System',
    Villager (..),
  )
import Control.Monad (when)
import Foreign.C (CFloat (..))
import Raylib.Core (getFrameTime)
import Raylib.Types (Rectangle (..), Vector2 (..))
import System.Random (randomRIO)
import Utils
  ( normalizeVector,
    vectorLength,
    (|*#|),
    (|+|),
    (|-|),
  )

defaultIdleInfo :: Vector2 -> IdleInfo
defaultIdleInfo position = IdleInfo position 0.0 (3.0, 5.0) 20.0 (Vector2 0.0 0.0)

updateVillagerCollision :: System' ()
updateVillagerCollision =
  cmapM_ $ \(Villager _ _, Sprite, Position (Vector2 x y), CollisionBox (Rectangle rx ry rw rh), ety) -> do
    set ety $ CollisionBox (Rectangle x y rw rh)

updateVillagerIdleState :: System' ()
updateVillagerIdleState =
  cmapM_ $ \(Villager _ _, idleInfo@(IdleInfo idlePos timer range radius target), Position pos, ety :: Entity) -> do
    delta <- liftIO getFrameTime
    let newTimer = updateIdleTick timer delta
    set ety (IdleInfo idlePos newTimer range radius target)
    when (newTimer <= 0.0) $ do
      newIdleInfo <- liftIO $ getNewTarget idleInfo
      set ety newIdleInfo
    let hasNotArrived = vectorLength (target |-| pos) > 1.0
    when (vectorLength (target |-| pos) > 1.0) $ do
      set ety (Position (moveVillager target pos delta))

updateIdleTick :: Float -> Float -> Float
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

moveVillager :: Vector2 -> Vector2 -> Float -> Vector2
moveVillager target pos delta = newPos
  where
    direction = target |-| pos
    normalizedDirection = normalizeVector direction
    newPos = pos |+| (normalizedDirection |*#| (50.0 * delta))
