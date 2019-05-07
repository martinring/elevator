class ElevatorView implements View {
  private group: SVGGElement = document.createElementNS('http://www.w3.org/2000/svg','g')

  public get elem(): SVGElement {
    return this.group
  }

  constructor (
    private model: Elevator,
    private svg: SVGSVGElement,
    private options: ViewOptions
  ) {    
    let rect: SVGRectElement = document.createElementNS('http://www.w3.org/2000/svg','rect')
    let buttons: SVGTextElement[] = []  
    let group = this.group
    this.group.appendChild(rect)
    let doorLeft: SVGRectElement = document.createElementNS('http://www.w3.org/2000/svg','rect')
    let doorRight: SVGRectElement = document.createElementNS('http://www.w3.org/2000/svg','rect')    
    this.group.appendChild(doorLeft)
    this.group.appendChild(doorRight)
    this.group.classList.add("elevator")
    rect.width.baseVal.value = options.elevatorWidth
    rect.height.baseVal.value = options.elevatorHeight
    doorLeft.width.baseVal.value = options.elevatorWidth / 2
    doorLeft.height.baseVal.value = options.elevatorHeight
    doorRight.x.baseVal.value = options.elevatorWidth / 2
    doorRight.width.baseVal.value = options.elevatorWidth / 2
    doorRight.height.baseVal.value = options.elevatorHeight
    doorLeft.classList.add('door')
    doorRight.classList.add('door')
    let position = svg.createSVGTransform()    
    let x = model.shaft.id * (options.elevatorWidth + options.elevatorGap)
    let base = model.shaft.building.floors.length * options.floorHeight - options.elevatorHeight
    position.setTranslate(x,base)
    this.group.transform.baseVal.initialize(position)    
    this.model.shaft.building.floors.forEach(floor => {
      let button = document.createElementNS('http://www.w3.org/2000/svg','text')
      button.classList.add('elevator-button')
      button.textContent = floor.id.toString()
      this.group.appendChild(button)
      let d = Math.min(options.elevatorWidth / 8, options.elevatorHeight / (floor.building.floorCount + 2))
      button.style.fontSize = d.toString()
      let pos = svg.createSVGTransform()
      pos.setTranslate(d,(model.shaft.building.floorCount + 2) * d - (floor.id + 1) * d)
      button.transform.baseVal.initialize(pos)
      buttons.push(button)
    })
    function pos(i) {
      let r = Math.ceil(Math.sqrt(model.capacity))
      let dx = options.elevatorWidth / (r + 1)
      let dy = options.elevatorHeight / (r + 1)
      return {
        x: dx + dx * Math.floor(i / r),
        y: dy + dy * (i % r)
      }
    } 
    model.door.listen({
      opened(): void {},
      closed(): void {},
      moved(position: number): void {
        let w = (1 - position) * options.elevatorWidth / 2
        doorLeft.width.baseVal.value = w
        doorRight.width.baseVal.value = w
        doorRight.x.baseVal.value = options.elevatorWidth - w
      },
      broken(reason: string): void {            
        doorLeft.classList.add('broken')
        doorRight.classList.add('broken')
      } 
    })
    this.model.listen({
      floorPassed(): void {},
      stopped(): void {},
      buttonPressed(floor: number): void {},
      broken(reason: string): void {},
      personEntered(person: Person, index): void {
        if (person.elem.parentElement) {
          person.elem.parentElement.removeChild(person.elem)
        }        
        let p = pos(index)
        let po = svg.createSVGTransform()
        po.setTranslate(p.x,p.y)
        person.elem.transform.baseVal.initialize(po)
        group.appendChild(person.elem)
      },
      moved(p: number): void {
        let y = base - p * options.floorHeight
        position.setTranslate(x,y)
        group.transform.baseVal.initialize(position)
      },
      lightStateChanged(floor: number): void {
        buttons[floor].classList.toggle('on')
      }
    })
  }
}