{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Collisions where

import Apecs
import Components (CollisionBox (..), DrawCollisions (..), System', Collision (..), CollisionType (..),)
import Control.Monad (unless, when)
import Foreign.C (CInt (..))
import Raylib (drawRectangle)
import Raylib.Types (Color (..), Rectangle (..))
import Tilemap (tileSize)
import Utils (areBoxesColliding)

toggleDrawCollision :: System' ()
toggleDrawCollision = cmap $ \(DrawCollisions enabled) -> DrawCollisions (not enabled)

drawCollisions :: System' ()
drawCollisions = do
  (DrawCollisions enabled) <- get global
  when enabled $ do
    cmapM $ \(Collision colType colliding _ _, CollisionBox rect@(Rectangle rx ry rw rh)) -> do
      let selectedColor
            | colliding && colType == Body = Color 230 41 55 170
            | not colliding && colType == Body = Color 0 121 241 170
            | colliding && colType == Trigger = Color 253 249 0 170
            | not colliding && colType == Trigger = Color 0 241 4 170
      liftIO $ drawRectangle (round rx) (round ry) tileSize tileSize selectedColor

detectCollisions :: System' ()
detectCollisions = do
  cmapM_ $ \(Collision colType1 colliding1 mOther1 checked1, cBox1@(CollisionBox rect1), ety1 :: Entity) ->
    cmapM_ $ \(Collision colType2 colliding2 mOther2 checked2, cBox2@(CollisionBox rect2), ety2 :: Entity) -> do
      when (ety1 /= ety2 && colType1 == colType2) $ do
        if areBoxesColliding rect1 rect2
          then do
            set ety1 (Collision colType1 True (Just ety2) True, cBox1)
            set ety2 (Collision colType2 True (Just ety1) True, cBox2)
          else do
            unless checked1 $ set ety1 (Collision colType1 False Nothing True, cBox1)
            unless checked2 $ set ety2 (Collision colType2 False Nothing True, cBox2)

  cmap $ \(Collision colType colliding mOther checked) -> Collision colType colliding mOther False
