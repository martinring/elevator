interface ElevatorEvents {
  floorPassed(): void;  
  stopped(): void;
  buttonPressed(floor: number): void;  
  broken(reason: string): void;
  personEntered(person: Person, index: number): void;  
  moved(position: number): void;
  lightStateChanged(floor: number): void;
}