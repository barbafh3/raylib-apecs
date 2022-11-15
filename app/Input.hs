module Input
  ( handleInput,
    setupGlobalInputActions,
    isKeyboardActionPressed,
    isKeyboardActionDown,
    isKeyboardActionReleased,
    isKeyboardActionUp,
    isMouseActionPressed,
    isMouseActionDown,
    isMouseActionReleased,
    isMouseActionUp,
  )
where

import Apecs
import Components
import Raylib
import Raylib.Constants
import Raylib.Types (Camera2D (..), Vector2 (..))

setupGlobalInputActions :: [KeyboardAction] -> [MouseAction] -> System' ()
setupGlobalInputActions kbActions mouseActions = set global $ InputList kbActions mouseActions

handleInput :: System' ()
handleInput = do
  (InputList kbActions mouseActions, entity) <- get global
  newKeyboardActionList <- handleKeyboardActionList kbActions
  newMouseActionList <- handleMouseActionList mouseActions
  set entity $ InputList newKeyboardActionList newMouseActionList

handleKeyboardActionList :: [KeyboardAction] -> System' [KeyboardAction]
handleKeyboardActionList [] = return []
handleKeyboardActionList [action] = do
  newAction <- handleKeyboardAction action
  return [newAction]
handleKeyboardActionList (action : actions) = do
  newAction <- handleKeyboardAction action
  newActionList <- handleKeyboardActionList actions
  return (newAction : newActionList)

handleKeyboardAction :: KeyboardAction -> System' KeyboardAction
handleKeyboardAction action@(KeyboardAction _ [] _) = return action
handleKeyboardAction action@(KeyboardAction name keys state) = do
  actionPressed <- liftIO $ handleKeyboardActionKeyPressed keys
  actionDown <- liftIO $ handleKeyboardActionKeyDown keys
  actionReleased <- liftIO $ handleKeyboardActionKeyReleased keys
  let newState
        | actionPressed = Pressed
        | actionDown = Down
        | actionReleased = Released
        | otherwise = Up
  return $ KeyboardAction name keys newState

handleKeyboardActionKeyPressed :: [Int] -> IO Bool
handleKeyboardActionKeyPressed [] = return False
handleKeyboardActionKeyPressed [key] = liftIO $ isKeyPressed key
handleKeyboardActionKeyPressed (key : keys) = do
  thisKey <- liftIO $ isKeyPressed key
  nextKeys <- liftIO $ handleKeyboardActionKeyPressed keys
  return (thisKey || nextKeys)

handleKeyboardActionKeyDown :: [Int] -> IO Bool
handleKeyboardActionKeyDown [] = return False
handleKeyboardActionKeyDown [key] = liftIO $ isKeyDown key
handleKeyboardActionKeyDown (key : keys) = do
  thisKey <- liftIO $ isKeyDown key
  nextKeys <- liftIO $ handleKeyboardActionKeyDown keys
  return (thisKey || nextKeys)

handleKeyboardActionKeyReleased :: [Int] -> IO Bool
handleKeyboardActionKeyReleased [] = return False
handleKeyboardActionKeyReleased [key] = liftIO $ isKeyReleased key
handleKeyboardActionKeyReleased (key : keys) = do
  thisKey <- liftIO $ isKeyReleased key
  nextKeys <- liftIO $ handleKeyboardActionKeyReleased keys
  return (thisKey || nextKeys)

handleKeyboardActionKeyUp :: [Int] -> IO Bool
handleKeyboardActionKeyUp [] = return False
handleKeyboardActionKeyUp [key] = liftIO $ isKeyUp key
handleKeyboardActionKeyUp (key : keys) = do
  thisKey <- liftIO $ isKeyUp key
  nextKeys <- liftIO $ handleKeyboardActionKeyUp keys
  return (thisKey || nextKeys)

isKeyboardActionDown :: KeyboardActionName -> System' Bool
isKeyboardActionDown name = do
  (InputList kbActions _) <- get global
  if getKeyboardActionState name kbActions == Down
    then return True
    else return False

isKeyboardActionPressed :: KeyboardActionName -> System' Bool
isKeyboardActionPressed name = do
  (InputList kbActions _) <- get global
  if getKeyboardActionState name kbActions == Pressed
    then return True
    else return False

isKeyboardActionUp :: KeyboardActionName -> System' Bool
isKeyboardActionUp name = do
  (InputList kbActions _) <- get global
  if getKeyboardActionState name kbActions == Up
    then return True
    else return False

isKeyboardActionReleased :: KeyboardActionName -> System' Bool
isKeyboardActionReleased name = do
  (InputList kbActions _) <- get global
  if getKeyboardActionState name kbActions == Released
    then return True
    else return False

getKeyboardActionState :: KeyboardActionName -> [KeyboardAction] -> InputState
getKeyboardActionState _ [] = Up
getKeyboardActionState name (KeyboardAction actionName keys state : actions)
  | name == actionName = state
  | otherwise = getKeyboardActionState name actions

handleMouseActionList :: [MouseAction] -> System' [MouseAction]
handleMouseActionList [] = return []
handleMouseActionList [action] = do
  newAction <- handleMouseAction action
  return [newAction]
handleMouseActionList (action : actions) = do
  newAction <- handleMouseAction action
  newActionList <- handleMouseActionList actions
  return (newAction : newActionList)

handleMouseAction :: MouseAction -> System' MouseAction
handleMouseAction action@(MouseAction _ [] _) = return action
handleMouseAction action@(MouseAction name keys state) = do
  actionPressed <- liftIO $ handleMouseActionKeyPressed keys
  actionDown <- liftIO $ handleMouseActionKeyDown keys
  actionReleased <- liftIO $ handleMouseActionKeyReleased keys
  let newState
        | actionPressed = Pressed
        | actionDown = Down
        | actionReleased = Released
        | otherwise = Up
  return $ MouseAction name keys newState

handleMouseActionKeyPressed :: [Int] -> IO Bool
handleMouseActionKeyPressed [] = return False
handleMouseActionKeyPressed [key] = liftIO $ isMouseButtonPressed key
handleMouseActionKeyPressed (key : keys) = do
  thisKey <- liftIO $ isKeyPressed key
  nextKeys <- liftIO $ handleMouseActionKeyPressed keys
  return (thisKey || nextKeys)

handleMouseActionKeyDown :: [Int] -> IO Bool
handleMouseActionKeyDown [] = return False
handleMouseActionKeyDown [key] = liftIO $ isMouseButtonDown key
handleMouseActionKeyDown (key : keys) = do
  thisKey <- liftIO $ isKeyDown key
  nextKeys <- liftIO $ handleMouseActionKeyDown keys
  return (thisKey || nextKeys)

handleMouseActionKeyReleased :: [Int] -> IO Bool
handleMouseActionKeyReleased [] = return False
handleMouseActionKeyReleased [key] = liftIO $ isMouseButtonReleased key
handleMouseActionKeyReleased (key : keys) = do
  thisKey <- liftIO $ isKeyReleased key
  nextKeys <- liftIO $ handleMouseActionKeyReleased keys
  return (thisKey || nextKeys)

handleMouseActionKeyUp :: [Int] -> IO Bool
handleMouseActionKeyUp [] = return False
handleMouseActionKeyUp [key] = liftIO $ isMouseButtonUp key
handleMouseActionKeyUp (key : keys) = do
  thisKey <- liftIO $ isKeyUp key
  nextKeys <- liftIO $ handleMouseActionKeyUp keys
  return (thisKey || nextKeys)

isMouseActionDown :: MouseActionName -> System' Bool
isMouseActionDown name = do
  (InputList _ mouseActions) <- get global
  if getMouseActionState name mouseActions == Down
    then return True
    else return False

isMouseActionPressed :: MouseActionName -> System' Bool
isMouseActionPressed name = do
  (InputList _ mouseActions) <- get global
  if getMouseActionState name mouseActions == Pressed
    then return True
    else return False

isMouseActionUp :: MouseActionName -> System' Bool
isMouseActionUp name = do
  (InputList _ mouseActions) <- get global
  if getMouseActionState name mouseActions == Up
    then return True
    else return False

isMouseActionReleased :: MouseActionName -> System' Bool
isMouseActionReleased name = do
  (InputList _ mouseActions) <- get global
  if getMouseActionState name mouseActions == Released
    then return True
    else return False

getMouseActionState :: MouseActionName -> [MouseAction] -> InputState
getMouseActionState _ [] = Up
getMouseActionState name (MouseAction actionName keys state : actions)
  | name == actionName = state
  | otherwise = getMouseActionState name actions
