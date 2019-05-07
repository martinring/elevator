{-# LANGUAGE DeriveGeneric,
             DuplicateRecordFields #-}

module Messages (
  Direction (
    Up,
    Down
  ), 
  Event (
    Initialize,
    ElevatorBroken,
    ElevatorDoorBroken,
    FloorDoorBroken,
    FloorPassed,
    ElevatorStopped,
    ElevatorDoorOpened,
    ElevatorDoorClosed,
    FloorDoorOpened,
    FloorDoorClosed,
    ElevatorButtonPressed,
    FloorButtonPressed
  ), 
  Command (
    MoveElevator,
    StopElevator,
    OpenElevatorDoor,
    CloseElevatorDoor,
    OpenFloorDoor,
    CloseFloorDoor,
    SetElevatorButtonLight,
    SetFloorButtonLight
  )
) where

import GHC.Generics

data Direction = Up | Down deriving (Generic, Show)

data Event = Initialize { floors :: Int, shafts :: Int, capacity :: Int }
           | ElevatorBroken { shaft :: Int, reason :: String }
           | ElevatorDoorBroken { shaft :: Int, reason :: String }
           | FloorDoorBroken { floor :: Int, shaft :: Int, reason :: String }
           | FloorPassed { shaft :: Int }
           | ElevatorStopped { shaft :: Int }
           | ElevatorDoorOpened { shaft :: Int }
           | ElevatorDoorClosed { shaft :: Int }
           | FloorDoorOpened { floor :: Int, shaft :: Int }
           | FloorDoorClosed { floor :: Int, shaft :: Int }
           | ElevatorButtonPressed { shaft :: Int, floor :: Int }                   
           | FloorButtonPressed { floor :: Int, direction :: Direction } deriving (Show, Generic)

data Command = MoveElevator { shaft :: Int, direction :: Direction }
             | StopElevator { shaft :: Int }
             | OpenElevatorDoor { shaft :: Int }
             | CloseElevatorDoor { shaft :: Int }
             | OpenFloorDoor { floor :: Int, shaft :: Int }
             | CloseFloorDoor { floor :: Int, shaft :: Int }
             | SetElevatorButtonLight { shaft :: Int, floor :: Int, on :: Bool }
             | SetFloorButtonLight { floor :: Int, direction :: Direction, on :: Bool } deriving (Show, Generic)