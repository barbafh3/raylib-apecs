{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE GADTs #-}
module Components where

import Apecs
import Linear (V2)
import Raylib.Types (Camera2D, Vector2, Rectangle(..))
import Foreign.C (CFloat)

-- GENERAL
data CameraComponent = CameraComponent CFloat Camera2D deriving Show

-- INPUT
data InputState = Pressed | Released | Down | Up deriving (Show, Eq)
data InputAction = InputAction String [Int] InputState deriving Show
newtype InputList = InputList [InputAction] deriving Show

-- UI
data UIElement = UIElement Vector2 Vector2 Int Bool deriving Show
data ButtonState = Normal | Hovered | Toggled | Held deriving (Show, Eq)
data TextureButton where
  Button :: Rectangle -> ButtonState -> (m ()) -> TextureButton

-- TILEMAP
data Tile = Tile Vector2 Rectangle deriving Show
data Chunk = Chunk Rectangle [Tile] Bool deriving Show
newtype Tilemap = Tilemap [Chunk] deriving Show

makeWorldAndComponents "World" [''CameraComponent, ''InputList, ''Tilemap, ''TextureButton, ''UIElement]

type System' a = System World a

initWorld' = initWorld
