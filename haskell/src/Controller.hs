module Controller (
  control
) where

import Control.Monad (forM_)
import Messages

type Send = Command -> IO ()

control :: Send -> Event -> IO ()
control send msg = case msg of
  Initialize floors shafts capacity -> do
    putStrLn $
      "initialized building with " <> show floors <>
      " floors, " <> show shafts <> " shafts and elevator capacity " <>
      show capacity
    forM_ [0..shafts-1] $ \shaft -> do 
      send (OpenElevatorDoor shaft)
      send (OpenFloorDoor 0 shaft)
  ElevatorBroken shaft reason ->
    return ()
  ElevatorDoorBroken shaft reason ->
    return ()
  FloorDoorBroken floor shaft reason ->
    return ()
  FloorPassed shaft ->
    return ()
  ElevatorStopped shaft ->
    return ()
  ElevatorDoorOpened shaft -> do
    putStrLn $ "elevator door " <> show shaft <> " opened"
    return ()
  ElevatorDoorClosed shaft ->
    return ()
  FloorDoorOpened floor shaft -> do
    putStrLn $ "floor door " <> show shaft <> " on floor " <> show floor <> " opened"
    return ()
  FloorDoorClosed floor shaft ->
    return ()
  ElevatorButtonPressed shaft floor ->
    return ()
  FloorButtonPressed floor direction ->
    return ()