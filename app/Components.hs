{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Components where

import Apecs
import Apecs.Core
import Foreign.C (CFloat)
import Language.Haskell.TH ()
import Language.Haskell.TH.Syntax
import Linear (V2)
import Raylib.Types (Camera2D, Rectangle (..), Texture, Vector2)

type RangeF = (CFloat, CFloat)

-- ------------------------------------------------------------------------------------------ DEBUG
newtype ShowFPS = ShowFPS Bool deriving (Show)

newtype DrawCollisions = DrawCollisions Bool deriving (Show)

-- ------------------------------------------------------------------------------------------ GENERAL
data CameraComponent = CameraComponent CFloat Camera2D deriving (Show)

data GameAtlasSets = GameAtlasSets Texture Texture deriving (Show)

data Sprite = Sprite Vector2 Rectangle deriving (Show)

-- ------------------------------------------------------------------------------------------ INPUT
data KeyboardActionName = MoveLeft | MoveRight | MoveUp | MoveDown deriving (Show, Eq)

data KeyboardAction = KeyboardAction KeyboardActionName [Int] InputState deriving (Show)

data MouseActionName = LeftClick | RightClick deriving (Show, Eq)

data MouseAction = MouseAction MouseActionName [Int] InputState deriving (Show)

data InputState = Pressed | Released | Down | Up deriving (Show, Eq)

data InputList = InputList [KeyboardAction] [MouseAction] deriving (Show)

-- ------------------------------------------------------------------------------------------ COLLISIONS
newtype CollisionBox = CollisionBox Rectangle deriving (Show)

data BodyCollision = BodyCollision Bool (Maybe Entity) Bool

data TriggerCollision = TriggerCollision Bool (Maybe Entity) Bool

-- ------------------------------------------------------------------------------------------ UI
data UIElement = UIElement Vector2 Vector2 Int Bool deriving (Show)

data ButtonState = Normal | Hovered | Toggled | Held deriving (Show, Eq)

-- data TextureButton w = TextureButtonF Rectangle ButtonState (System w ())
data TextureButton = TextureButton Rectangle ButtonState ButtonAction deriving (Show)

data ButtonAction
  = ToggleFPSAction
  | ToggleDrawCollisionAction
  deriving (Show, Eq)

-- ------------------------------------------------------------------------------------------ TILEMAP
data Tile = Tile Vector2 Rectangle deriving (Show)

data Chunk = Chunk Rectangle [Tile] Bool deriving (Show)

newtype Tilemap = Tilemap [Chunk] deriving (Show)

-- ------------------------------------------------------------------------------------------ VILLAGERS
data VillagerType = Hauler | Builder deriving (Show, Eq)

data VillagerState = Idle | Loading | Carrying | Working deriving (Show, Eq)

data Villager = Villager VillagerType VillagerState deriving (Show)

data IdleInfo = IdleInfo Vector2 CFloat RangeF CFloat Vector2 deriving (Show)

-- ------------------------------------------------------------------------------------------ BUILDINGS
data BuildingType = House | Warehouse deriving (Show, Eq)

data BuildingState = Enabled | Disabled deriving (Show, Eq)

data Building = Building BuildingType BuildingState deriving (Show)

-- ------------------------------------------------------------------------------------------ MAKE WORLD
makeWorldAndComponents
  "World"
  [ ''CameraComponent,
    ''GameAtlasSets,
    ''InputList,
    ''Tilemap,
    ''TextureButton,
    ''UIElement,
    ''ShowFPS,
    ''DrawCollisions,
    ''CollisionBox,
    ''BodyCollision,
    ''TriggerCollision,
    ''Sprite,
    ''Villager,
    ''IdleInfo
  ]

type System' a = SystemT World IO a

initWorld' :: IO World
initWorld' = initWorld
