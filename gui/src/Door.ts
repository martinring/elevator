/// <reference path="./Model.ts" />

class Door extends BreakableModel<DoorEvents> {
  private speed = 0.7 + Math.random() * 0.6
  private state_ = 0 
  private opening_: boolean = false
  private closing_: boolean = false

  constructor () {
    super()
  }
  
  public get state(): number {
    return this.state_
  }

  public get broken() {
    return this.broken_
  }

  public get closing() {
    return this.closing_
  }

  public get opening() {
    return this.opening_
  }

  open(): void {
    if (!this.broken) {
      if (this.opening) {
        this.break("requested to open door while door was already opening")
      } else if (this.closing) {
        this.break("requested to open door while door was closing")
      } else if (this.state_ == 1) {
        this.break("requested to open door, when door was already open")
      } else {
        this.opening_ = true
      }
    }
  }
  
  close(): void {
    if (!this.broken) {
      if (this.opening) {
        this.break("requested to close door while door was opening")
      } else if (this.closing) {
        this.break("requested to close door while door was already closing")
      } else if (this.state_ == 0) {
        this.break("requested to close door, when door was already closed")
      } else {
        this.closing_ = true
      }
    }
  }

  public progress(dt: number) {
    if (this.opening) {
      this.state_ = Math.min(1,this.state + dt * this.speed)
      if (this.state == 1) {
        this.opening_ = false
        this.listeners.forEach(l => l.opened())
      }
      this.listeners.forEach(l => l.moved(this.state))
    } else if (this.closing) {
      this.state_ = Math.max(0,this.state - dt * this.speed)
      if (this.state == 0) {
        this.closing_ = false
        this.listeners.forEach(l => l.closed())
      }
      this.listeners.forEach(l => l.moved(this.state))
    }
  }
}