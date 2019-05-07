interface ElevatorControl {
  move(direction: Direction): void;
  stop(): void;
  openDoor(): void;
  closeDoor(): void;  
  setLight(floor: number, on: boolean): void;
}