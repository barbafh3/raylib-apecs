{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
module Components where

import Apecs
import Linear (V2)
import Raylib.Types (Camera2D, Vector2, Rectangle)

newtype Position = Position (V2 Double) deriving Show
newtype CameraComponent = CameraComponent Camera2D deriving Show

data InputState = Pressed | Released | Down | Up deriving Show
data InputAction = InputAction String [Int] InputState deriving Show
newtype InputList = InputList [InputAction] deriving Show

-- TILEMAP
data Tile = Tile Vector2 Rectangle deriving Show
data Chunk = Chunk Rectangle [Tile] Bool deriving Show
newtype Tilemap = Tilemap [Chunk] deriving Show

makeWorldAndComponents "World" [''Position, ''CameraComponent, ''InputList, ''Tilemap]

type System' a = System World a

initWorld' = initWorld
