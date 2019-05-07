interface FloorEvents {
  buttonPressed(direction: Direction): void
  personEntered(person: Person, index: number): void  
  lightStateChanged(direction: Direction, on: Boolean): void
}