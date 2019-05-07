import akka.actor.{Actor, ActorLogging, ActorRef}

class Controller extends Actor with ActorLogging {
  def receive = waitingForClient

  def waitingForClient: Receive = {
    case client: ActorRef =>
      context.become(waitingForInitialization(client))
  }

  def waitingForInitialization(client: ActorRef): Receive = {
    case Event.Initialize(floors, shafts, capacity) =>
      log.info(s"initialized building with $floors floors, $shafts shafts and elevator capacity $capacity")
      context.become(running(client,floors,shafts,capacity))
      for (i <- 0 until shafts) {
        client ! Command.OpenElevatorDoor(i)
        client ! Command.OpenFloorDoor(0,i)
      }
  }

  def running(client: ActorRef, floors: Int, shafts: Int, capacity: Int): Receive = {
    case Event.ElevatorBroken(shaft, reason) =>
    case Event.ElevatorDoorBroken(shaft, reason) =>
    case Event.FloorDoorBroken(floor, shaft, reason) =>
    case Event.FloorPassed(shaft) =>
    case Event.ElevatorStopped(shaft) =>
    case Event.ElevatorDoorOpened(shaft) =>
    case Event.ElevatorDoorClosed(shaft) =>
    case Event.FloorDoorOpened(floor, shaft) =>
    case Event.FloorDoorClosed(floor, shaft) =>
    case Event.ElevatorButtonPressed(shaft, floor) =>
      client ! Command.SetElevatorButtonLight(shaft,floor,true)
    case Event.FloorButtonPressed(floor, direction) =>
  }
}
