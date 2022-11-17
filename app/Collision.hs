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
    cmapM $ \(BodyCollision colliding _ _, CollisionBox rect@(Rectangle rx ry rw rh), Not :: Not TriggerCollision) -> do
      let selectedColor
            | colliding = Color 230 41 55 170
            | otherwise = Color 0 121 241 170
      liftIO $ drawRectangle (round rx) (round ry) tileSize tileSize selectedColor
    cmapM_ $ \(TriggerCollision colliding _ _, CollisionBox rect@(Rectangle rx ry rw rh)) -> do
      let selectedColor
            | colliding = Color 253 249 0 170
            | otherwise = Color 0 121 241 170
      liftIO $ drawRectangle (round rx) (round ry) tileSize tileSize selectedColor

detectBodyCollisions :: System' ()
detectBodyCollisions = do
  cmapM_ $ \(BodyCollision colliding1 mOther1 checked1, cBox1@(CollisionBox rect1), ety1 :: Entity) ->
    cmapM_ $ \(BodyCollision colliding2 mOther2 checked2, cBox2@(CollisionBox rect2), ety2 :: Entity) -> do
      when (ety1 /= ety2) $ do
        if areBoxesColliding rect1 rect2
          then do
            set ety1 (BodyCollision True (Just ety2) True, cBox1)
            set ety2 (BodyCollision True (Just ety1) True, cBox2)
          else do
            unless checked1 $ set ety1 (BodyCollision False Nothing True, cBox1)
            unless checked2 $ set ety2 (BodyCollision False Nothing True, cBox2)

  cmapM_ $ \(BodyCollision colliding mOther checked, ety :: Entity) -> set ety (BodyCollision colliding mOther False)

detectTriggerCollisions :: System' ()
detectTriggerCollisions = do
  cmapM_ $ \(TriggerCollision colliding1 mOther1 checked1, cBox1@(CollisionBox rect1), ety1 :: Entity) ->
    cmapM_ $ \(TriggerCollision colliding2 mOther2 checked2, cBox2@(CollisionBox rect2), ety2 :: Entity) -> do
      when (ety1 /= ety2) $ do
        if areBoxesColliding rect1 rect2
          then do
            set ety1 (TriggerCollision True (Just ety2) True, cBox1)
            set ety2 (TriggerCollision True (Just ety1) True, cBox2)
          else do
            unless checked1 $ set ety1 (TriggerCollision False Nothing True, cBox1)
            unless checked2 $ set ety2 (TriggerCollision False Nothing True, cBox2)

  cmapM_ $ \(TriggerCollision colliding mOther checked, ety :: Entity) -> set ety (TriggerCollision colliding mOther False)