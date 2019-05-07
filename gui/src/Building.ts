class Building {
  private floors_: Floor[] = []
  private shafts_: Shaft[] = []

  constructor (public floorCount: number, public shaftCount: number) {
    for (let i = 0; i < floorCount; i++) {
      this.floors.push(new Floor(i,this))
    }
    for (let i = 0; i < shaftCount; i++) {
      this.shafts.push(new Shaft(i,this,12))
    }
  }

  public get floors() {
    return this.floors_
  }

  public get shafts() {
    return this.shafts_
  }  
}