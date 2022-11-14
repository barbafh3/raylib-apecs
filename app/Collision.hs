{-# LANGUAGE ScopedTypeVariables #-}
module Collision where
import Components (System', DrawCollisions (..), BodyCollision (..), CollisionBox (..), TriggerCollision (..))
import Apecs
import Control.Monad (when)
import Raylib.Types (Rectangle(..), Color (..))

toggleDrawCollision :: System' ()
toggleDrawCollision = cmapM_ $ \(DrawCollisions enabled, ety) -> set ety $ DrawCollisions (not enabled)

drawCollisions :: System' ()
drawCollisions = 
    cmapM_ $ \(DrawCollisions enabled) -> 
        when enabled $ do
            let drawnEntities :: [Entity] = []
            cmapM_ $ \(BodyCollision colliding _, CollisionBox rect@(Rectangle rx ry rw rh), ety :: Entity) -> do
                let selectedColor
                        | colliding = Color 230 41 55 170
                        | otherwise = Color 0 121 241 170
                let drawnEntities = ety : drawnEntities
                return ()
            liftIO $ print $ show drawnEntities
            cmapM_ $ \(TriggerCollision colliding _, CollisionBox rect@(Rectangle rx ry rw rh), ety :: Entity) -> do
                return ()
        
