/// <reference path="./Model.ts" />

class Elevator extends BreakableModel<ElevatorEvents> implements ElevatorControl {
  constructor(
    public shaft: Shaft,
    private capacity_: number
  ) {
    super()
  }

  private direction: Direction | null = null
  private stopRequested: boolean = false
  private door_ = new Door()
  private position_: number = 0
  private velocity: number = 0
  private lights: boolean[] = new Array(this.shaft.building.floors.length)

  public lightState(floor: number): boolean {
    if (floor > 0 && floor < this.lights.length) return this.lights[Math.floor(floor)]
    else return false
  }

  public get capacity(): number {
    return this.capacity_
  }

  private people: Array<boolean> = new Array(this.capacity)
  private peopleCount: number = 0

  public enter(person: Person): number | null {
    if (this.peopleCount < this.capacity) {
      const i = this.people.findIndex(x => !x)
      this.people[i] = true
      this.peopleCount += 1
      this.listeners.forEach(l => l.personEntered(person,i))
      return i
    } else {
      return null
    }
  }

  public leave(i: number) {
    this.people[i] = false
    this.peopleCount -= 1
  }

  public get broken() {
    return this.broken_
  }

  public get door() {
    return this.door_
  }

  public get position() {
    return this.position_
  }

  move(direction: Direction): void {
    if (!this.broken) {
      if (this.direction != null) {
        this.break("requested movement before elevator stopped")        
      } else if (this.door.closing) {
        this.break("requested to move while door was closing")
      } else if (this.door.opening) {
        this.break("requested to move while door was opening")
      } else if (this.door.state != 0) {
        this.break("requested movement while door was not closed")
      } else {
        this.direction = direction
      }
    }    
  }

  stop(): void {    
    if (!this.broken) {
      if (this.stopRequested) {
        this.break("requested to stop while already stopping")
      } if (this.direction == null) {
        this.break("requested to stop when elevator was already stopped")
      } else {
        this.stopRequested = true
      }
    }
  }

  openDoor(): void {
    if (!this.broken) {
      if (this.direction != null) {
        this.break("requested to open door while elevator was moving")
      } else {
        this.door.open()
      }
    }
  }
  
  closeDoor(): void {
    if (!this.broken) {
      if (this.direction != null) {
        this.break("requested to close door while elevator was moved")
      } else {
        this.door.close()
      }
    }
  }

  setLight(floor: number, on: boolean): void {
    if (this.lights[floor] != on) {
      this.lights[floor] = on
      this.listeners.forEach(l => l.lightStateChanged(floor))
    }
  }

  public pressButton(floor: number) {
    this.listeners.forEach(l => l.buttonPressed(floor))
  }

  public progress(dt: number) {
    if (!this.broken) {
      this.door.progress(dt)
      let p = this.position
      if (this.direction == Direction.Up) {      
        p += dt
        if (this.stopRequested && Math.floor(p) != Math.floor(this.position)) {
          p = Math.floor(p)
          this.direction = null
          this.stopRequested = false
          this.listeners.forEach(l => l.stopped())
        }
      }    
      else if (this.direction == Direction.Down) {
        p -= dt    
        if (this.stopRequested && Math.floor(p) != Math.floor(this.position)) {
          p = Math.floor(this.position)
          this.direction = null
          this.stopRequested = false
          this.listeners.forEach(l => l.stopped())
        }
      }
      if (Math.round(this.position) != Math.round(p)) {
        this.listeners.forEach(l => l.floorPassed())
      }
      if (p != this.position_) this.listeners.forEach(l => l.moved(p))
      this.position_ = p
      if (this.position > this.shaft.building.floors.length - 0.5) this.break("elevator moved passed seiling")
      if (this.position < 0) this.break("elevator moved below basement")
    } else {
      let p = this.position_
      if (p != -0.5 || this.velocity != 0) {              
        this.velocity -= dt * 9.81
        p += this.velocity * dt
        if (this.position < -0.5) {
          p = -0.5
          this.velocity = -this.velocity * 0.8
          if (this.velocity < 1) {
            this.velocity = 0            
          }
        }
        if (p != this.position_) this.listeners.forEach(l => l.moved(p))
        this.position_ = p
      }
    }
  }  
}