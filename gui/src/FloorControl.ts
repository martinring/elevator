interface FloorControl {  
  setLight(direction: Direction, on: boolean): void
  setDoorLight(shaft: number, direction: Direction | null): void
}