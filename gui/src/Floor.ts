/// <reference path="./Model.ts" />

class Floor extends Model<FloorEvents> implements FloorControl {
  constructor (
    public id: number,
    private building_: Building
  ) {
    super()
    for (let i = 0; i < building_.shaftCount; i++) {
      this.doors_.push(new Door())
    }
  }

  public get building(): Building {
    return this.building_
  }

  private doors_: Door[] = []
  private lightUp_: boolean
  private lightDown_: boolean

  public lightState(direction: Direction): boolean {
    if (direction == Direction.Up) return this.lightUp_
    else return this.lightDown_
  }

  public door(shaft: number): Door {    
    return this.doors_[shaft]         
  }

  private people: Array<boolean> = new Array(0)
  private peopleCount: number = 0

  public enter(person: Person): number {
    if (this.peopleCount == this.people.length) {
      this.people.push(true)
      const i = this.peopleCount
      this.peopleCount = i + 1
      this.listeners.forEach(l => l.personEntered(person,i))
      return i
    } else {
      const i = this.people.findIndex(x => !x)
      this.people[i] = true
      this.peopleCount += 1
      this.listeners.forEach(l => l.personEntered(person,i))
      return i
    }
  }

  public leave(i: number) {
    this.people[i] = false
    this.peopleCount -= 1
  }

  pressButton(direction: Direction) {
    this.listeners.forEach(l => l.buttonPressed(direction))
  }

  setLight(direction: Direction, on: boolean): void {    
    if (direction == Direction.Up && this.lightUp_ != on) {
      this.lightUp_ = on
      this.listeners.forEach(l => l.lightStateChanged(direction,on)) 
    } if (direction == Direction.Down && this.lightDown_ != on) {
      this.lightDown_ = on
      this.listeners.forEach(l => l.lightStateChanged(direction,on)) 
    }    
  }

  setDoorLight(shaft: number, direction: Direction | null): void {
    throw new Error("Method not implemented.");
  }

  progress(dt: number): void {
    this.doors_.forEach((door,i) => {
      door.progress(dt)
      if (!door.broken && door.state > 0 && this.building.shafts[i].elevator.position != this.id) {
        door.break("door not fully closed while elevator not behind door")
        this.building.shafts[i].elevator.break("elevator not behind open door")
      }
    })
  }
}