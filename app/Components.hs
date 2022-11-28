{-# LANGUAGE DeriveAnyClass #-}
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
import Data.HashMap (HashMap)
import qualified Data.HashMap as HM
import Data.Hashable
import Foreign.C (CFloat)
import Foreign.C.Types (CInt)
import Language.Haskell.TH ()
import Language.Haskell.TH.Syntax
import Linear (V2)
import Raylib.Types (Camera2D, Color, Font, Rectangle (..), Texture, Vector2)

type RangeF = (CFloat, CFloat)

-- ------------------------------------------------------------------------------------------ DEBUG
newtype ShowFPS = ShowFPS Bool deriving (Show)

newtype DrawCollisions = DrawCollisions Bool deriving (Show)

-- ------------------------------------------------------------------------------------------ GENERAL
newtype Parent = Parent Entity

newtype Position = Position Vector2

newtype Offset = Offset Vector2

newtype Layer = Layer CInt

newtype Scale = Scale CFloat

newtype Visibility = Visibility Bool

data CameraComponent = CameraComponent CFloat Camera2D deriving (Show)

newtype FontsComponent = FontsComponent Font deriving (Show)

data GameAtlasSets = GameAtlasSets Texture Texture deriving (Show)

data Sprite = Sprite Vector2 Rectangle deriving (Show)

newtype GlobalStorageList = GlobalStorageList ResourceStorage deriving (Show)

-- ------------------------------------------------------------------------------------------ INPUT
data KeyboardActionName = MoveLeft | MoveRight | MoveUp | MoveDown | AddStone deriving (Show, Eq)

data KeyboardAction = KeyboardAction KeyboardActionName [Int] InputState deriving (Show)

data MouseActionName = LeftClick | RightClick deriving (Show, Eq)

data MouseAction = MouseAction MouseActionName [Int] InputState deriving (Show)

data InputState = Pressed | Released | Down | Up deriving (Show, Eq)

data InputList = InputList [KeyboardAction] [MouseAction] deriving (Show)

-- ------------------------------------------------------------------------------------------ COLLISIONS
newtype CollisionBox = CollisionBox Rectangle deriving (Show)

data CollisionType = Trigger | Body deriving (Show, Eq)

-- data BodyCollision = BodyCollision Bool (Maybe Entity) Bool
-- data TriggerCollision = TriggerCollision Bool (Maybe Entity) Bool
data Collision = Collision CollisionType Bool (Maybe Entity) Bool deriving (Show)

-- ------------------------------------------------------------------------------------------ UI
data UIElement = UIElement

data ToggleButton = ToggleButton

data ButtonState = Normal | Hovered | Toggled | Held deriving (Show, Eq)

data TextureButton = TextureButton Rectangle ButtonState ButtonAction deriving (Show)

data ButtonAction
  = ToggleFPSAction
  | ToggleDrawCollisionAction
  | ToggleResourcePanel
  | ToggleBuildingPanel
  deriving (Show, Eq)

newtype DrawResourcePanel = DrawResourcePanel Bool deriving (Show)

newtype DrawBuildingPanel = DrawBuildingPanel Bool deriving (Show)

data Label = Label String Float Float Color deriving (Show)

data GlobalStorageLabel = GlobalStorageLabel

data GlobalWoodLabel = GlobalWoodLabel

data GlobalStoneLabel = GlobalStoneLabel

data GlobalPlankLabel = GlobalPlankLabel

data GlobalStoneBrickLabel = GlobalStoneBrickLabel

data ResourcePanel = ResourcePanel

newtype UIImage = UIImage Rectangle deriving (Show)

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

type StorageItem = (String, Int)

type ResourceStorage = HM.Map String Int

data StorageSpace = StorageSpace ResourceStorage ResourceStorage deriving (Show)

data ConstructionSpace = ConstructionSpace Bool ResourceStorage deriving (Show)

-- ------------------------------------------------------------------------------------------ MAKE WORLD
makeWorldAndComponents
  "World"
  [ ''Parent,
    ''Position,
    ''Offset,
    ''Layer,
    ''Scale,
    ''Visibility,
    ''CameraComponent,
    ''FontsComponent,
    ''GameAtlasSets,
    ''InputList,
    ''Tilemap,
    ''TextureButton,
    ''ToggleButton,
    ''Label,
    ''UIElement,
    ''UIImage,
    ''GlobalStorageLabel,
    ''GlobalWoodLabel,
    ''GlobalStoneLabel,
    ''GlobalPlankLabel,
    ''GlobalStoneBrickLabel,
    ''GlobalStorageList,
    ''ShowFPS,
    ''DrawCollisions,
    ''DrawResourcePanel,
    ''DrawBuildingPanel,
    ''ResourcePanel,
    ''CollisionBox,
    ''Collision,
    ''Sprite,
    ''Villager,
    ''IdleInfo,
    ''StorageSpace,
    ''ConstructionSpace
  ]

type System' a = SystemT World IO a

initWorld' :: IO World
initWorld' = initWorld
