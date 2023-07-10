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
import Raylib.Types
  ( Camera2D,
    Color,
    Font,
    KeyboardKey,
    MouseButton,
    Rectangle (..),
    Texture,
    Vector2,
  )

type RangeF = (Float, Float)

-- ------------------------------------------------------------------------------------------ DEBUG
newtype ShowFPS = ShowFPS Bool deriving (Show)

newtype DrawCollisions = DrawCollisions Bool deriving (Show)

-- ------------------------------------------------------------------------------------------ GENERAL
newtype Parent = Parent Entity deriving (Show)

newtype Position = Position Vector2 deriving (Show)

newtype Offset = Offset Vector2 deriving (Show)

newtype Layer = Layer CInt deriving (Show)

newtype Scale = Scale Float deriving (Show)

newtype Visibility = Visibility Bool deriving (Show)

newtype AtlasRegion = AtlasRegion Rectangle deriving (Show)

data CameraComponent = CameraComponent Float Camera2D deriving (Show)

newtype FontsComponent = FontsComponent Font deriving (Show)

data GameAtlasSets = GameAtlasSets {mapTileset :: Texture, uiAtlas :: Texture} deriving (Show)

data Sprite = Sprite deriving (Show)

newtype GlobalStorageList = GlobalStorageList ResourceStorage deriving (Show)

data Scene = MainMenu | TestMap deriving (Show, Eq)

newtype ActiveScene = ActiveScene Scene deriving (Show)

-- ------------------------------------------------------------------------------------------ INPUT
data KeyboardActionName
  = MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown
  | AddStone
  deriving (Show, Eq)

data KeyboardAction = KeyboardAction KeyboardActionName [KeyboardKey] InputState deriving (Show)

data MouseActionName = LeftClick | RightClick deriving (Show, Eq)

data MouseAction = MouseAction MouseActionName [MouseButton] InputState deriving (Show)

data InputState = Pressed | Released | Down | Up deriving (Show, Eq)

data InputList = InputList [KeyboardAction] [MouseAction] deriving (Show)

-- ------------------------------------------------------------------------------------------ COLLISIONS
newtype CollisionBox = CollisionBox Rectangle deriving (Show)

data CollisionType = Trigger | Body deriving (Show, Eq)

data TriggerCollision = TriggerCollision deriving (Show)

data BodyCollision = BodyCollision deriving (Show, Eq)

data Collision = Collision {colliding :: Bool, mOther :: Maybe Entity, checked :: Bool} deriving (Show)

-- ------------------------------------------------------------------------------------------ UI
data UIElement = UIElement

data ToggleButton = ToggleButton

data ButtonState = Normal | Hovered | Toggled | Held deriving (Show, Eq)

data TextureButton = TextureButton ButtonState ButtonAction deriving (Show)

data ButtonAction
  = ToggleFPSAction
  | ToggleDrawCollisionAction
  | ToggleResourcePanel
  | ToggleBuildingPanel
  | MainMenuStart
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

data IdleInfo = IdleInfo
  { idlePosition :: Vector2,
    idleTimer :: Float,
    idleTimerRange :: RangeF,
    idleRadius :: Float,
    idleTargetPosition :: Vector2
  }
  deriving (Show)

-- ------------------------------------------------------------------------------------------ BUILDINGS
data BuildingType = House | Warehouse deriving (Show, Eq)

data BuildingState = Enabled | Disabled deriving (Show, Eq)

data Building = Building
  { buildingType :: BuildingType,
    buildingState :: BuildingState
  }
  deriving (Show)

type StorageItem = (String, Int)

type ResourceStorage = HM.Map String Int

data StorageSpace = StorageSpace
  { storedResources :: ResourceStorage,
    reservedSpace :: ResourceStorage
  }
  deriving (Show)

data ConstructionSpace = ConstructionSpace
  { tasksGenerated :: Bool,
    requiredResources :: ResourceStorage
  }
  deriving (Show)

-- ------------------------------------------------------------------------------------------ TASKS

data HaulTask = HaulTask
  { originPosition :: Maybe Vector2,
    destinationPosition :: Vector2
  }
  deriving (Show)

newtype BuildTask = BuildTask Vector2 deriving (Show)

data OpenTasks = OpenTasks
  { openHaulTasks :: [HaulTask],
    openBuildTasks :: [BuildTask]
  }
  deriving (Show)

-- ------------------------------------------------------------------------------------------ MAKE WORLD
makeWorldAndComponents
  "World"
  [ -- GENERAL
    ''Parent,
    ''Position,
    ''Offset,
    ''Layer,
    ''Scale,
    ''Visibility,
    ''CameraComponent,
    ''FontsComponent,
    ''GameAtlasSets,
    ''AtlasRegion,
    ''ActiveScene,
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
    ''BodyCollision,
    ''TriggerCollision,
    ''Sprite,
    ''Villager,
    ''IdleInfo,
    ''StorageSpace,
    ''ConstructionSpace,
    ''OpenTasks,
    ''HaulTask
  ]

type System' a = SystemT World IO a

initWorld' :: IO World
initWorld' = initWorld
