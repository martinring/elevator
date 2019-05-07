class Person {
  private position_: Floor | Elevator;
  public index_: number;
  private destination: number | null;
  private id_: number;
  private static idCounter: number = 0;

  constructor(private building: Building, public elem: SVGGraphicsElement, private speedFactor: number) {
    this.position_ = building.floors[0]
    this.index_ = this.position_.enter(this)
    this.act(0)
    this.id_ = Person.idCounter++;    
  }

  private waitingSince: number = 0

  public get id(): number {
    return this.id_
  }

  public get waitTime(): number {
    return Math.min(1, (Date.now() - this.waitingSince) / (60000 / this.speedFactor))
  }

  public get position(): Floor | Elevator {
    return this.position_
  }  

  public get index(): number {
    return this.index_
  }

  public act(delay: number) {
    window.setTimeout(() => {      
      if (!this.destination) {
        this.waitingSince = Date.now()
        this.destination = Math.floor(Math.random() * this.building.floors.length)
        this.elem.style.fill = "#0f0"
        this.act(2000)
      } else if (this.position instanceof Floor) {
        let r = Math.min(255,512 * this.waitTime)
        let g = Math.min(255,512 - 512 * this.waitTime)
        this.elem.style.fill = `rgb(${r},${g},0)`
        if (this.destination == this.position.id) {
          this.destination = null
        } else {
          let f: Floor = this.position
          this.building.shafts.forEach(shaft => {
            if (!(this.position_ instanceof Elevator) && shaft.elevator.position == f.id && shaft.elevator.door.state > 0.8 && f.door(shaft.id).state > 0.8) {
              const i = shaft.elevator.enter(this)
              if (i != null) {
                this.waitingSince = Date.now()
                this.destination = Math.floor(Math.random() * this.building.floors.length)
                this.elem.style.fill = "#0f0"        
                this.position_.leave(this.index)
                this.position_ = shaft.elevator
                this.index_ = i                
              }
            }
          })
          let dir = this.destination < this.position.id ? Direction.Down : Direction.Up
          if (!this.position.lightState(dir)) 
            this.position.pressButton(dir)
        }
        this.act(500)
      } else {        
        let r = Math.min(255,512 * this.waitTime)
        let g = Math.min(255,512 - 512 * this.waitTime)
        this.elem.style.fill = `rgb(${r},${g},0)`
        if (!this.position.lightState(this.destination))
          this.position.pressButton(this.destination)
        if (this.position.door.state > 0.5 && this.position.position == this.destination) {
          this.position.leave(this.index)
          this.position_ = this.building.floors[this.destination]
          this.index_ = this.position_.enter(this)
          this.destination = null
          this.elem.style.fill = "#fff"
        }
        this.act(500)
      } 
    }, (delay + Math.random() * 500) / this.speedFactor)
  }
}