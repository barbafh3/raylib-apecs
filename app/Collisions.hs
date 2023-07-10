{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Collisions where

import Apecs
import Components
  ( BodyCollision (..),
    Collision (..),
    CollisionBox (..),
    DrawBuildingPanel (..),
    DrawCollisions (..),
    DrawResourcePanel (..),
    System',
    TriggerCollision (..),
  )
import Control.Monad (unless, when)
import Foreign.C (CInt (..))
import Raylib.Core.Shapes (drawRectangle)
import Raylib.Types (Color (..), Rectangle (..))
import Tilemap (tileSize)
import Utils (areBoxesColliding)

toggleDrawCollision :: System' ()
toggleDrawCollision = cmap $ \(DrawCollisions enabled) -> DrawCollisions (not enabled)

drawCollisions :: System' ()
drawCollisions = do
  (DrawCollisions enabled) <- get global
  when enabled $ do
    cmapM $ \(BodyCollision, Collision colliding _ _, CollisionBox rect@(Rectangle rx ry rw rh)) -> do
      let selectedColor
            | colliding = Color 230 41 55 170
            | not colliding = Color 0 121 241 170
      liftIO $ drawRectangle (round rx) (round ry) tileSize tileSize selectedColor
    cmapM $ \(TriggerCollision, Collision colliding _ _, CollisionBox rect@(Rectangle rx ry rw rh)) -> do
      let selectedColor
            | colliding = Color 253 249 0 170
            | not colliding = Color 0 241 4 170
      liftIO $ drawRectangle (round rx) (round ry) tileSize tileSize selectedColor

detectCollisions :: System' ()
detectCollisions = do
  cmapM_ $ \(BodyCollision, Collision colliding1 mOther1 checked1, cBox1@(CollisionBox rect1), ety1 :: Entity) ->
    cmapM_ $ \(BodyCollision, Collision colliding2 mOther2 checked2, cBox2@(CollisionBox rect2), ety2 :: Entity) -> do
      when (ety1 /= ety2) $ do
        if areBoxesColliding rect1 rect2
          then do
            set ety1 (Collision True (Just ety2) True, cBox1)
            set ety2 (Collision True (Just ety1) True, cBox2)
          else do
            unless checked1 $ set ety1 (Collision False Nothing True, cBox1)
            unless checked2 $ set ety2 (Collision False Nothing True, cBox2)

  cmapM_ $ \(TriggerCollision, Collision colliding1 mOther1 checked1, cBox1@(CollisionBox rect1), ety1 :: Entity) ->
    cmapM_ $ \(TriggerCollision, Collision colliding2 mOther2 checked2, cBox2@(CollisionBox rect2), ety2 :: Entity) -> do
      when (ety1 /= ety2) $ do
        if areBoxesColliding rect1 rect2
          then do
            set ety1 (Collision True (Just ety2) True, cBox1)
            set ety2 (Collision True (Just ety1) True, cBox2)
          else do
            unless checked1 $ set ety1 (Collision False Nothing True, cBox1)
            unless checked2 $ set ety2 (Collision False Nothing True, cBox2)

  cmap $ \(Collision colliding mOther checked) -> Collision colliding mOther False
