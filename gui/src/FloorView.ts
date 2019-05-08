class FloorView implements View {
  private group: SVGGElement = document.createElementNS('http://www.w3.org/2000/svg','g')  

  public get elem(): SVGElement {
    return this.group
  }

  constructor (
    private model: Floor,
    private svg: SVGSVGElement,
    private options: ViewOptions
  ) {        
    let group = this.group
    let floorWidth = 
      options.textWidth + options.elevatorGap +
      model.building.shafts.length * (options.elevatorGap + options.elevatorWidth)  
    let floors =
      model.building.floors.length
    let leftDoors = new Array<SVGRectElement>(this.model.building.shafts.length)
    let rightDoors = new Array<SVGRectElement>(this.model.building.shafts.length)
    for (let i = 0; i < model.building.shafts.length; i++) {
      let door = this.model.door(i)
      let left = document.createElementNS('http://www.w3.org/2000/svg','rect')
      let right = document.createElementNS('http://www.w3.org/2000/svg','rect')
      leftDoors[i] = left
      rightDoors[i] = right
      left.classList.add('door')
      right.classList.add('door')
      left.height.baseVal.value = options.elevatorHeight
      right.height.baseVal.value = options.elevatorHeight
      left.width.baseVal.value = options.elevatorWidth / 2
      right.width.baseVal.value = options.elevatorWidth / 2
      left.y.baseVal.value = -options.elevatorHeight
      right.y.baseVal.value = -options.elevatorHeight
      left.x.baseVal.value = options.textWidth + options.elevatorGap + (options.elevatorWidth + options.elevatorGap) * i
      right.x.baseVal.value = options.textWidth + options.elevatorGap + (options.elevatorWidth / 2) + (options.elevatorWidth + options.elevatorGap) * i
      this.group.appendChild(left)
      this.group.appendChild(right)
      door.listen({
        opened(): void {},
        closed(): void {},
        broken(reason: string): void {
          left.classList.add('broken')
          right.classList.add('broken')
        },
        moved(pos: number): void {
          let w = (1 - pos) * options.elevatorWidth / 2      
          leftDoors[i].width.baseVal.value = w
          rightDoors[i].width.baseVal.value = w
          rightDoors[i].x.baseVal.value = options.textWidth + options.elevatorGap + (options.elevatorWidth - w) + (options.elevatorWidth + options.elevatorGap) * i          
        }
      })
    }
    let separator = document.createElementNS('http://www.w3.org/2000/svg','line')
    separator.classList.add('floor-separator')
    let wall = document.createElementNS('http://www.w3.org/2000/svg','path')
    wall.classList.add('wall')
    let d = `M 0 0h${options.textWidth + options.elevatorGap}`
    for (let e = 0; e < model.building.shafts.length; e++) {         
      d += `v${-options.elevatorHeight}h${options.elevatorWidth}v${options.elevatorHeight}h${options.elevatorGap}`
    }
    d += `v${-options.floorHeight}h${-floorWidth}z`    
    wall.setAttribute("d",d)    
    this.group.appendChild(wall)
    this.group.appendChild(separator)
    separator.x1.baseVal.value = 0
    separator.y1.baseVal.value = 0
    separator.x2.baseVal.value = floorWidth
    separator.y2.baseVal.value = 0
    let label = document.createElementNS('http://www.w3.org/2000/svg','text')
    label.textContent = "E" + model.id
    label.classList.add('floor-label')
    let pos = svg.createSVGTransform()
    pos.setTranslate((options.textWidth + options.elevatorGap) / 3,- options.elevatorHeight / 2)
    label.transform.baseVal.initialize(pos)    
    this.group.appendChild(label)
    let up = document.createElementNS('http://www.w3.org/2000/svg','path') 
    let upd = `M${(options.textWidth + options.elevatorGap) / 3} ${- 3.3 * options.elevatorHeight / 4}`   
    upd += `l-10 10h20z`
    up.setAttribute('d',upd)
    up.classList.add('button', 'up')
    this.group.appendChild(up)   
    let down = document.createElementNS('http://www.w3.org/2000/svg','path')  
    let downd = `M${(options.textWidth + options.elevatorGap) / 3} ${- options.elevatorHeight / 4}`
    downd += `l-10 -10h20z`
    down.setAttribute('d',downd)
    down.classList.add('button', 'down')
    this.group.appendChild(down)    

    let transform = svg.createSVGTransform()
    transform.setTranslate(0,(floors - model.id) * options.floorHeight)
    this.group.transform.baseVal.initialize(transform)

    function p(i: number) {
      let r = Math.floor(options.floorHeight / 10) - 1
      let dx = 10
      let dy = 10
      return {
        x: dx + dx * Math.floor(i / r),
        y: dy + dy * (i % r)
      }      
    }

    this.model.listen({
      buttonPressed(direction: Direction): void {},
      personEntered(person: Person, index): void {        
        if (person.elem.parentElement) 
          person.elem.parentElement.removeChild(person.elem)
        let t = svg.createSVGTransform()
        let po = p(index)
        t.setTranslate(options.textWidth + options.elevatorGap - po.x,-po.y)
        person.elem.transform.baseVal.initialize(t)
        group.appendChild(person.elem)
      },
      lightStateChanged(direction: Direction, on: Boolean): void {        
         if (direction == Direction.Up) up.classList.toggle('on')
         else down.classList.toggle('on')
      }
    })
  }
}