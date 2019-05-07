

sealed trait Direction
object Direction {
  case object Up extends Direction
  case object Down extends Direction
}

sealed trait Event
object Event {
  case class Initialize(floors: Int, shafts: Int, capacity: Int) extends Event
  case class ElevatorBroken(shaft: Int, reason: String) extends Event
  case class ElevatorDoorBroken(shaft: Int, reason: String) extends Event
  case class FloorDoorBroken(floor: Int, shaft: Int, reason: String) extends Event
  case class FloorPassed(shaft: Int) extends Event
  case class ElevatorStopped(shaft: Int) extends Event
  case class ElevatorDoorOpened(shaft: Int) extends Event
  case class ElevatorDoorClosed(shaft: Int) extends Event
  case class FloorDoorOpened(floor: Int, shaft: Int) extends Event
  case class FloorDoorClosed(floor: Int, shaft: Int) extends Event
  case class ElevatorButtonPressed(shaft: Int, floor: Int) extends Event
  case class FloorButtonPressed(floor: Int, direction: Direction) extends Event
}

sealed trait Command
object Command {
  case class MoveElevator(shaft: Int, direction: Direction) extends Command
  case class StopElevator(shaft: Int) extends Command
  case class OpenElevatorDoor(shaft: Int) extends Command
  case class CloseElevatorDoor(shaft: Int) extends Command
  case class OpenFloorDoor(floor: Int, shaft: Int) extends Command
  case class CloseFloorDoor(floor: Int, shaft: Int) extends Command
  case class SetElevatorButtonLight(shaft: Int, floor: Int, on: Boolean) extends Command
  case class SetFloorButtonLight(floor: Int, direction: Direction, on: Boolean) extends Command
}
