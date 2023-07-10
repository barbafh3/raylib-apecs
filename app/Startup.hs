module Startup where

import Apecs
import Building.Warehouse (newWarehouse)
import Components
  ( ActiveScene (..),
    AtlasRegion (..),
    ButtonAction (..),
    CameraComponent (..),
    CollisionType (..),
    DrawCollisions (..),
    FontsComponent (..),
    GameAtlasSets (..),
    GlobalStorageList (..),
    InputState (..),
    KeyboardAction (..),
    KeyboardActionName (..),
    MouseAction (..),
    MouseActionName (..),
    OpenTasks (..),
    Position (..),
    Scene (..),
    ShowFPS (..),
    Sprite (..),
    System',
  )
import Constants (mainFontPath, screenHeightCF, screenHeightF, tilesetPath, uiAtlasPath)
import qualified Data.HashMap as Map
import Input (gameKeyboardActions, gameMouseActions, setupGlobalInputActions)
import Raylib.Core.Text (loadFont)
import Raylib.Core.Textures (loadTexture)
import Raylib.Types (Camera2D (..), Font, Rectangle (..), Vector2 (..))
import Raylib.Util (WindowResources)
import Tilemap (generateTilemap, tileSizeF)
import UI (newTextureButton, newToggleTextureButton, testMapUiStartup)
import Villager.Hauler (newHauler)

gameStartup :: WindowResources -> System' ()
gameStartup window = do
  mainFont <- coreSetup window
  pickActiveScene TestMap

coreSetup :: WindowResources -> System' ()
coreSetup window = do
  setupGlobalInputActions gameKeyboardActions gameMouseActions

  localTileset <- liftIO $ loadTexture tilesetPath window
  localUiAtlas <- liftIO $ loadTexture uiAtlasPath window
  set global $ GameAtlasSets localTileset localUiAtlas

  mainFont <- liftIO $ loadFont mainFontPath window
  set global $ FontsComponent mainFont

  generateTilemap 1024 1024

  set global $ ShowFPS False
  set global $ DrawCollisions False

  set global $ GlobalStorageList $ Map.fromList [("Wood", 50)]

  let camera = Camera2D (Vector2 0.0 0.0) (Vector2 0.0 0.0) 0.0 2.0
  set global $ CameraComponent 10.0 camera

pickActiveScene :: Scene -> System' ()
pickActiveScene scene
  | scene == TestMap = testMapStartup scene
  | scene == MainMenu = mainMenuStartup scene -- TODO: Create main menu scene

testMapStartup :: Scene -> System' ()
testMapStartup scene = do
  set global $ ActiveScene scene
  testMapUiStartup
  testMapEntitySetup

testMapEntitySetup :: System' ()
testMapEntitySetup = do
  set global $ OpenTasks [] []

  let idlePoint = Vector2 304.0 176.0

  -- Flag Sprite at IdlePoint
  -- _ <- newEntity (Position idlePoint, Sprite, AtlasRegion (Rectangle (2.0 * tileSizeCF) (6.0 * tileSizeCF) tileSizeCF tileSizeCF))
  _ <- newEntity (Position idlePoint, Sprite, AtlasRegion (Rectangle (2.0 * tileSizeF) (6.0 * tileSizeF) tileSizeF tileSizeF))

  _ <- newHauler (Vector2 250.0 50.0) idlePoint Trigger
  _ <- newHauler (Vector2 250.0 250.0) idlePoint Trigger

  _ <- newWarehouse (Vector2 208.0 48.0) (Just $ Map.singleton "Wood" 50)

  return ()

mainMenuStartup :: Scene -> System' ()
mainMenuStartup scene = do
  set global $ ActiveScene scene

  newTextureButton (Vector2 10.0 $ screenHeightF - 10.0) 0 (Vector2 0.0 3.0) (Just (Vector2 48 16)) MainMenuStart

  return ()
