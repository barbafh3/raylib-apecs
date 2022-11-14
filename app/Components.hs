{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE PatternSynonyms       #-}

module Components where

import Apecs
import Foreign.C (CFloat)
import Language.Haskell.TH ()
import Linear (V2)
import Raylib.Types (Camera2D, Rectangle (..), Vector2)
import Language.Haskell.TH.Syntax
import Apecs.Core


-- DEBUG
newtype ShowFPS = ShowFPS Bool deriving Show
newtype DrawCollisions = DrawCollisions Bool deriving Show

-- GENERAL
data CameraComponent = CameraComponent CFloat Camera2D deriving (Show)

-- INPUT
data InputState = Pressed | Released | Down | Up deriving (Show, Eq)

data InputAction = InputAction String [Int] InputState deriving (Show)

newtype InputList = InputList [InputAction] deriving (Show)

-- COLLISION
newtype CollisionBox = CollisionBox Rectangle deriving Show

data BodyCollision = BodyCollision Bool (Maybe Entity)
data TriggerCollision = TriggerCollision Bool (Maybe Entity)

-- UI
data UIElement = UIElement Vector2 Vector2 Int Bool deriving (Show)

data ButtonState = Normal | Hovered | Toggled | Held deriving (Show, Eq)

-- data TextureButton w = TextureButtonF Rectangle ButtonState (System w ())
data TextureButton = TextureButton Rectangle ButtonState ButtonAction deriving Show

data ButtonAction  =
  ToggleFPSAction |
  ToggleDrawCollisionAction 
  deriving (Show, Eq)

-- TILEMAP
data Tile = Tile Vector2 Rectangle deriving (Show)

data Chunk = Chunk Rectangle [Tile] Bool deriving (Show)

newtype Tilemap = Tilemap [Chunk] deriving (Show)

makeWorldAndComponents "World" [
  ''CameraComponent, ''InputList, ''Tilemap, ''TextureButton, ''UIElement, ''ShowFPS,
  ''DrawCollisions, ''CollisionBox, ''BodyCollision, ''TriggerCollision
  ]

type System' a = SystemT World IO a

initWorld' :: IO World
initWorld' = initWorld

-- pattern TextureButton :: Rectangle -> ButtonState -> System World () -> TextureButton World
-- pattern TextureButton x y z = TextureButtonF x y z
