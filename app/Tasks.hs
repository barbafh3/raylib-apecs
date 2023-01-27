{-# LANGUAGE ScopedTypeVariables #-}

module Tasks where

import Apecs
import Components (HaulTask (..), OpenTasks (..), System', Villager (..), VillagerState (..))
import Control.Monad (when)
import Data.Foldable (find)
import Data.Maybe (isJust, isNothing)
import Raylib.Types (Vector2)
import Utils (getAndRemoveItemFromList)

generateHaulTask :: Maybe Vector2 -> Vector2 -> System' ()
generateHaulTask mOrigin destination = do
  (OpenTasks haulList buildList) <- get global
  let haul = HaulTask mOrigin destination
  set global $ OpenTasks (haul : haulList) buildList

updateTasks :: System' ()
updateTasks = do
  findIdleHaulerForHaulTask
  findStorageSourceForHaulTask

findIdleHaulerForHaulTask :: System' ()
findIdleHaulerForHaulTask = do
  (OpenTasks haulList buildList) <- get global
  let (mHaul, newHaulList) = getAndRemoveItemFromList (isJust . originPosition) haulList
  case mHaul of
    Just haul -> do
      cmapM_ $ \(Villager vType state, ety :: Entity) ->
        when (state == Idle) $ set ety (Villager vType Loading, haul)
      set global $ OpenTasks newHaulList buildList
    Nothing -> return ()

findStorageSourceForHaulTask :: System' ()
findStorageSourceForHaulTask = do
  return ()
