class Shaft {
  private elevator_: Elevator

  constructor (
    public id: number,
    public building: Building,
    private capacity: number
  ) {
    this.elevator_ = new Elevator(this,capacity)
  }
  
  public get elevator() {
    return this.elevator_
  }
}