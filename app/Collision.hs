{-# LANGUAGE ScopedTypeVariables #-}

module Collision where

import Apecs
import Components (BodyCollision (..), CollisionBox (..), DrawCollisions (..), System', TriggerCollision (..))
import Control.Monad (unless, when)
import Foreign.C (CInt (..))
import Raylib (drawRectangle)
import Raylib.Types (Color (..), Rectangle (..))
import Tilemap (tileSize)
import Utils (areBoxesColliding)

toggleDrawCollision :: System' ()
toggleDrawCollision = do
  liftIO $ print "Draw collision toggled"
  cmapM_ $ \(DrawCollisions enabled, ety) -> set ety $ DrawCollisions (not enabled)

drawCollisions :: System' ()
drawCollisions = do
  (DrawCollisions enabled) <- get global
  when enabled $ do
    cmapM $ \(BodyCollision colliding _, CollisionBox rect@(Rectangle rx ry rw rh), Not :: Not TriggerCollision) -> do
      let selectedColor
            | colliding = Color 230 41 55 170
            | otherwise = Color 0 121 241 170
      liftIO $ drawRectangle (round rx) (round ry) tileSize tileSize selectedColor
    cmapM_ $ \(TriggerCollision colliding _, CollisionBox rect@(Rectangle rx ry rw rh)) -> do
      let selectedColor
            | colliding = Color 253 249 0 170
            | otherwise = Color 0 121 241 170
      liftIO $ drawRectangle (round rx) (round ry) tileSize tileSize selectedColor

detectBodyCollisions :: System' ()
detectBodyCollisions = do
  cmapM_ $ \(BodyCollision colliding1 mOther1, cBox1@(CollisionBox rect1), ety1 :: Entity) ->
    cmapM_ $ \(BodyCollision colliding1 mOther2, cBox2@(CollisionBox rect2), ety2 :: Entity) -> do
      when (ety1 /= ety2 && areBoxesColliding rect1 rect2) $ do
        set ety1 (BodyCollision True (Just ety2), cBox1)
        set ety2 (BodyCollision True (Just ety1), cBox2)

detectTriggerCollisions :: System' ()
detectTriggerCollisions = do
  cmapM_ $ \(TriggerCollision colliding1 mOther1, cBox1@(CollisionBox rect1), ety1 :: Entity) ->
    cmapM_ $ \(TriggerCollision colliding2 mOther2, cBox2@(CollisionBox rect2), ety2 :: Entity) -> do
      when (ety1 /= ety2 && areBoxesColliding rect1 rect2) $ do
        set ety1 (TriggerCollision True (Just ety2), cBox1)
        set ety2 (TriggerCollision True (Just ety1), cBox2)