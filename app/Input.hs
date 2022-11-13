module Input (handleInput, setupInputActions, isActionDown) where

import Apecs
import Raylib.Constants
import Raylib
import Components (System', InputList (..), InputAction (..), InputState (..), CameraComponent (..))
import Raylib.Types (Camera2D (..), Vector2 (..))

setupInputActions :: [InputAction] -> System' ()
setupInputActions actions = do
  _ <- newEntity $ InputList actions
  return ()

handleInput :: System' ()
handleInput = do
  cmapM $ \(InputList actions, entity) -> do
    newActionList <- handleActionList actions
    set entity $ InputList newActionList


handleActionList :: [InputAction] -> System' [InputAction]
handleActionList [] = return []
handleActionList [action] = do
  newAction <- handleAction action
  return [newAction]
handleActionList (action: actions) = do
  newAction <- handleAction action
  newActionList <- handleActionList actions
  return (newAction : newActionList)


handleAction :: InputAction -> System' InputAction
handleAction action@(InputAction _ [] _) = return action
handleAction action@(InputAction name keys state) = do
  actionPressed <- liftIO $ handleActionKeyPressed keys
  actionReleased <- liftIO $ handleActionKeyReleased keys
  if actionPressed then do
    return $ InputAction name keys Pressed
  else if actionReleased then do
    return $ InputAction name keys Released
  else case state of
    Released -> return $ InputAction name keys Up
    Pressed -> return $ InputAction name keys Down
    _ -> return action

handleActionKeyPressed :: [Int] -> IO Bool
handleActionKeyPressed [] = return False
handleActionKeyPressed [key] = liftIO $ isKeyPressed key
handleActionKeyPressed (key: keys) = do
  thisKey <- liftIO $ isKeyPressed key
  nextKeys <- liftIO $ handleActionKeyPressed keys
  return (thisKey || nextKeys)

handleActionKeyReleased :: [Int] -> IO Bool
handleActionKeyReleased [] = return False
handleActionKeyReleased [key] = liftIO $ isKeyReleased key
handleActionKeyReleased (key: keys) = do
  thisKey <- liftIO $ isKeyReleased key
  nextKeys <- liftIO $ handleActionKeyReleased keys
  return (thisKey || nextKeys)

isActionDown :: String -> [InputAction] -> Bool
isActionDown _ [] = False
isActionDown name (InputAction actionName keys state : actions) 
  | name == actionName && (state == Down || state == Pressed) = True
  | otherwise = isActionDown name actions

