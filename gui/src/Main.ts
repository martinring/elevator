

function init() {
  let svg: SVGSVGElement = (() => {
    let elem = document.querySelector('svg')
    if (!elem) throw new Error("could not find root svg element")
    return elem
  })()
  const params = new URLSearchParams(window.location.search)
  let n = Number.parseInt(params.get('shafts') || '5') || 5// number of elevators
  let floors = Number.parseInt(params.get('floors') || '6') || 6
  let speed = Number.parseFloat(params.get('speed') || '1.0') || 1.0
  let capacity = Number.parseInt(params.get('capacity') || '12') || 12
  let socket = new WebSocket("ws://" + location.host)
  function send(event: any) {
    if (socket) socket.send(JSON.stringify(event))
    else console.error("socket was closed when trying to send message", event)
  }

  let options: ViewOptions = {
    textWidth: 200,
    floorHeight: 200,
    elevatorHeight: 150,
    elevatorWidth: 100,
    elevatorGap: 50
  }

  let floorWidth = options.textWidth + options.elevatorGap + n * (options.elevatorWidth + options.elevatorGap)
  let building = new Building(floors,n)
  svg.setAttribute('viewBox',"0 0 0 0")
  svg.viewBox.baseVal.width = floorWidth
  svg.viewBox.baseVal.height = floors * options.floorHeight
  let elevators = document.createElementNS('http://www.w3.org/2000/svg','g')
  let pos = svg.createSVGTransform()
  pos.setTranslate(options.textWidth + options.elevatorGap,0)
  elevators.transform.baseVal.initialize(pos)
  let buildingElem = document.createElementNS('http://www.w3.org/2000/svg','rect')
  buildingElem.width.baseVal.value = floorWidth
  buildingElem.height.baseVal.value = options.floorHeight * floors
  buildingElem.classList.add('building')
  let floorsElem = document.createElementNS('http://www.w3.org/2000/svg','g')
  //let peopleElem = document.createElementNS('http://www.w3.org/2000/svg','g')
  svg.appendChild(buildingElem)
  svg.appendChild(elevators)
  svg.appendChild(floorsElem)
  //svg.appendChild(peopleElem)
  let i = 0
  let interval = window.setInterval(() => {
    let elem = document.createElementNS('http://www.w3.org/2000/svg','circle')
    elem.r.baseVal.value = 4
    elem.classList.add('person')
    let p = new Person(building,elem,speed)
    if (i >= 228) window.clearInterval(interval)
    i++    
  },250 / speed)  
  /*for (let p = 0; p < 100; p++) {
    let person = new Person(building)
    people.push(person)
    let view = document.createElementNS('http://www.w3.org/2000/svg','circle')
    peopleElem.appendChild(view)
    view.classList.add('person')
    view.r.baseVal.value = 4
    let oldPosition: Elevator | Floor | null = null   
    let t = svg.createSVGTransform()
    view.transform.baseVal.initialize(t)
    view.cy.baseVal.value = - (Math.random() * 0.8 + 0.1) * options.elevatorHeight
    updateHandlers.push((dt: number) => {
      if (person.position instanceof Elevator) {        
        t.setTranslate(0,options.floorHeight * building.floorCount - (person.position.position * options.floorHeight))        
      } else {
        t.setTranslate(0,options.floorHeight * building.floorCount - person.position.id * options.floorHeight)
      }
      if (person.position != oldPosition) {
        oldPosition = person.position
        if (person.position instanceof Elevator) {
          view.cx.baseVal.value = 
            options.textWidth + options.elevatorGap + (Math.random() * 0.4 + 0.55) * options.elevatorWidth+ 
            person.position.shaft.id * (options.elevatorWidth + options.elevatorGap)
        } else if (person.position instanceof Floor) {
          view.cx.baseVal.value = 
            Math.random() * (options.textWidth + options.elevatorGap)           
        }
      }
    })
  } */ 
  building.shafts.forEach(shaft => {
    let view = new ElevatorView(shaft.elevator,svg,options)    
    elevators.appendChild(view.elem)
    shaft.elevator.listen({
      broken(reason: string) { 
        send({
          tag: "ElevatorBroken",
          "shaft": shaft.id,
          "reason": reason          
        })
      },
      floorPassed() {         
        send({
          tag: "FloorPassed",
          "shaft": shaft.id          
        })
      },
      stopped(): void {
        send({
          tag: "ElevatorStopped",
            "shaft": shaft.id
        })
      },
      buttonPressed(floor: number): void { 
        send({
          tag: "ElevatorButtonPressed",
            "shaft": shaft.id,
            "floor": floor
        })       
      },
      personEntered(): void {},
      moved(): void {},
      lightStateChanged(): void {}
    })
    shaft.elevator.door.listen({
      opened(): void { 
        send({
          tag: "ElevatorDoorOpened",
            "shaft": shaft.id
        })
      },
      closed(): void { 
        send({
          tag: "ElevatorDoorClosed",
            "shaft": shaft.id
        })
      },
      broken(reason: string) { 
        send({
          tag: "ElevatorDoorBroken",
            "shaft": shaft.id,
            "reason": reason
        })        
      },
      moved(): void {}
    })    
  })
  
  building.floors.forEach(floor => {
    let view = new FloorView(floor,svg,options)
    floorsElem.appendChild(view.elem)
    floor.listen({
      buttonPressed(direction: Direction) {
        send({
          tag: "FloorButtonPressed",
          "floor": floor.id,
          "direction": direction == Direction.Up ? "Up" : "Down"
        })
      },
      personEntered() {},      
      lightStateChanged() {}
    })    
    for (let i = 0; i < building.shaftCount; i++) {
      let j = i
      floor.door(j).listen({
        opened() {
          send({
            tag: "FloorDoorOpened",
            floor: floor.id,
            shaft: j
          })
        },        
        closed() {
          send({
            tag: "FloorDoorClosed",
            floor: floor.id,
            shaft: j            
          })
        },        
        broken(message) {
          send({
            tag: "FloorDoorBroken",
            "floor": floor.id,
            "shaft": j,
            "reason": message            
          })
        },
        moved() {}
      })
    }

  })
  let t0 = 0
  let base = options.floorHeight * floors - options.elevatorHeight;
  function loop(dt: number) {      
    building.shafts.forEach(shaft => {
      shaft.elevator.progress(dt)      
    })
    building.floors.forEach(floor => {
      floor.progress(dt)
    })
  }  
  let renderLoop = (t: number) => {
    let dt = t - t0
    t0 = t    
    loop(Math.min(0.1,speed * (dt / 1000)))
    window.requestAnimationFrame(renderLoop)
  }
  window.requestAnimationFrame(renderLoop)
  socket.addEventListener('open', e => {
    send({
      "tag": "Initialize",
      "floors": floors,
      "shafts": n,
      "capacity": capacity,
      "speed": speed
    })
  })  
  socket.addEventListener('message',e => {
    if (e.data != "ping") {
      let msg = JSON.parse(e.data)
      console.log("received message", msg)
      if (msg["shaft"] && (msg["shaft"] < 0 || msg["shaft"] >= building.shaftCount))
        console.error(`shaft ${msg["shaft"]} does not exist`, msg)
      else if (msg["floor"] && (msg["floor"] < 0 || msg["floor"] >= building.floorCount))
        console.error(`floor ${msg["floor"]} does not exist`, msg)
      else {
        switch (msg["tag"]) {
          case "MoveElevator":
            //(shaft: Int, direction: Direction)
            building.shafts[msg["shaft"]].elevator.move(msg["direction"] == "Up" ? Direction.Up : Direction.Down)
            break;
          case "StopElevator":
            building.shafts[msg["shaft"]].elevator.stop()     
            //(shaft: Int)
            break;
          case "OpenElevatorDoor":
            //(shaft: Int)          
            building.shafts[msg["shaft"]].elevator.openDoor()      
            break;
          case "CloseElevatorDoor":
            //(shaft: Int)          
            building.shafts[msg["shaft"]].elevator.closeDoor()                
            break;
          case "OpenFloorDoor":
            building.floors[msg["floor"]].door(msg["shaft"]).open()
            break;
          case "CloseFloorDoor":
            building.floors[msg["floor"]].door(msg["shaft"]).close()
            break;
          case "SetElevatorButtonLight":
            building.shafts[msg["shaft"]].elevator.setLight(msg["floor"],msg["on"])            
            break;
          case "SetFloorButtonLight":
            //(floor: Int, direction: Direction, on: Boolean)
            building.floors[msg["floor"]].setLight(msg["direction"] == "Up" ? Direction.Up : Direction.Down, msg["on"])
            break;
          default:
            console.warn("received unrecognised message", msg)
            break;
        }
      }
    } else socket.send("pong")    
  })
}

document.addEventListener("DOMContentLoaded",init,{
  once: true
})