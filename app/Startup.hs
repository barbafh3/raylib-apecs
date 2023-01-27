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
import Constants (mainFontPath, screenHeightCF, tilesetPath, uiAtlasPath)
import qualified Data.HashMap as Map
import Input (gameKeyboardActions, gameMouseActions, setupGlobalInputActions)
import Raylib (loadFont, loadTexture)
import Raylib.Types (Camera2D (..), Font, Rectangle (..), Vector2 (..))
import Tilemap (generateTilemap, tileSizeCF)
import UI (newTextureButton, newToggleTextureButton, testMapUiStartup)
import Villager.Hauler (newHauler)

gameStartup :: System' Font
gameStartup = do
  mainFont <- coreSetup
  pickActiveScene TestMap
  return mainFont

coreSetup :: System' Font
coreSetup = do
  setupGlobalInputActions gameKeyboardActions gameMouseActions

  localTileset <- liftIO $ loadTexture tilesetPath
  localUiAtlas <- liftIO $ loadTexture uiAtlasPath
  set global $ GameAtlasSets localTileset localUiAtlas

  mainFont <- liftIO $ loadFont mainFontPath
  set global $ FontsComponent mainFont

  generateTilemap 1024 1024

  set global $ ShowFPS False
  set global $ DrawCollisions False

  set global $ GlobalStorageList $ Map.fromList [("Wood", 50)]

  let camera = Camera2D (Vector2 0.0 0.0) (Vector2 0.0 0.0) 0.0 2.0
  set global $ CameraComponent 10.0 camera
  return mainFont

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
  _ <- newEntity (Position idlePoint, Sprite, AtlasRegion (Rectangle (2.0 * tileSizeCF) (6.0 * tileSizeCF) tileSizeCF tileSizeCF))

  _ <- newHauler (Vector2 250.0 50.0) idlePoint Trigger
  _ <- newHauler (Vector2 250.0 250.0) idlePoint Trigger

  _ <- newWarehouse (Vector2 208.0 48.0) (Just $ Map.singleton "Wood" 50)

  return ()

mainMenuStartup :: Scene -> System' ()
mainMenuStartup scene = do
  set global $ ActiveScene scene

  newTextureButton (Vector2 10.0 $ screenHeightCF - 10.0) 0 (Vector2 0.0 3.0) (Just (Vector2 48 16)) MainMenuStart

  return ()
