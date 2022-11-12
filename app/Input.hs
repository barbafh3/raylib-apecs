module Input (handleInput, setupInputActions) where

import Apecs
import Raylib.Constants
import Raylib
import Components (System', InputList (..), InputAction (..), InputState (..))

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
  -- liftIO $ print $ "Action '" ++ name ++ "' state: " ++ show state
  actionPressed <- liftIO $ actionKeyPressed keys
  actionReleased <- liftIO $ actionKeyReleased keys
  if actionPressed then do
    return $ InputAction name keys Pressed
  else if actionReleased then do
    return $ InputAction name keys Released
  else case state of
    Released -> return $ InputAction name keys Up
    Pressed -> return $ InputAction name keys Down
    _ -> return action


actionKeyPressed :: [Int] -> IO Bool
actionKeyPressed [] = return False
actionKeyPressed [key] = liftIO $ isKeyPressed key
actionKeyPressed (key: keys) = do
  thisKey <- liftIO $ isKeyPressed key
  nextKeys <- liftIO $ actionKeyPressed keys
  return (thisKey || nextKeys)

actionKeyReleased :: [Int] -> IO Bool
actionKeyReleased [] = return False
actionKeyReleased [key] = liftIO $ isKeyReleased key
actionKeyReleased (key: keys) = do
  thisKey <- liftIO $ isKeyReleased key
  nextKeys <- liftIO $ actionKeyReleased keys
  return (thisKey || nextKeys)

