abstract class Model<Events> {
  protected listeners: Array<Events> = new Array()

  public listen(listener: Events) {
    this.listeners.push(listener)
  }

  abstract progress(dt: number): void
}

abstract class BreakableModel<Events extends { broken(reason: string): void }> extends Model<Events> {
  protected broken_: boolean = false
  public get broken() {
    return this.broken_
  }
  public break(reason: string) {
    if (!this.broken) {
      this.broken_ = true
      this.listeners.forEach(l => l.broken(reason))
    }
  }
}