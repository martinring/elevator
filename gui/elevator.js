var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
var Building = /** @class */ (function () {
    function Building(floorCount, shaftCount) {
        this.floorCount = floorCount;
        this.shaftCount = shaftCount;
        this.floors_ = [];
        this.shafts_ = [];
        for (var i = 0; i < floorCount; i++) {
            this.floors.push(new Floor(i, this));
        }
        for (var i = 0; i < shaftCount; i++) {
            this.shafts.push(new Shaft(i, this, 12));
        }
    }
    Object.defineProperty(Building.prototype, "floors", {
        get: function () {
            return this.floors_;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(Building.prototype, "shafts", {
        get: function () {
            return this.shafts_;
        },
        enumerable: true,
        configurable: true
    });
    return Building;
}());
var Direction;
(function (Direction) {
    Direction[Direction["Up"] = 0] = "Up";
    Direction[Direction["Down"] = 1] = "Down";
})(Direction || (Direction = {}));
var Model = /** @class */ (function () {
    function Model() {
        this.listeners = new Array();
    }
    Model.prototype.listen = function (listener) {
        this.listeners.push(listener);
    };
    return Model;
}());
var BreakableModel = /** @class */ (function (_super) {
    __extends(BreakableModel, _super);
    function BreakableModel() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.broken_ = false;
        return _this;
    }
    Object.defineProperty(BreakableModel.prototype, "broken", {
        get: function () {
            return this.broken_;
        },
        enumerable: true,
        configurable: true
    });
    BreakableModel.prototype.break = function (reason) {
        if (!this.broken) {
            this.broken_ = true;
            this.listeners.forEach(function (l) { return l.broken(reason); });
        }
    };
    return BreakableModel;
}(Model));
/// <reference path="./Model.ts" />
var Door = /** @class */ (function (_super) {
    __extends(Door, _super);
    function Door() {
        var _this = _super.call(this) || this;
        _this.speed = 0.7 + Math.random() * 0.6;
        _this.state_ = 0;
        _this.opening_ = false;
        _this.closing_ = false;
        return _this;
    }
    Object.defineProperty(Door.prototype, "state", {
        get: function () {
            return this.state_;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(Door.prototype, "broken", {
        get: function () {
            return this.broken_;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(Door.prototype, "closing", {
        get: function () {
            return this.closing_;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(Door.prototype, "opening", {
        get: function () {
            return this.opening_;
        },
        enumerable: true,
        configurable: true
    });
    Door.prototype.open = function () {
        if (!this.broken) {
            if (this.opening) {
                this.break("requested to open door while door was already opening");
            }
            else if (this.closing) {
                this.break("requested to open door while door was closing");
            }
            else if (this.state_ == 1) {
                this.break("requested to open door, when door was already open");
            }
            else {
                this.opening_ = true;
            }
        }
    };
    Door.prototype.close = function () {
        if (!this.broken) {
            if (this.opening) {
                this.break("requested to close door while door was opening");
            }
            else if (this.closing) {
                this.break("requested to close door while door was already closing");
            }
            else if (this.state_ == 0) {
                this.break("requested to close door, when door was already closed");
            }
            else {
                this.closing_ = true;
            }
        }
    };
    Door.prototype.progress = function (dt) {
        var _this = this;
        if (this.opening) {
            this.state_ = Math.min(1, this.state + dt * this.speed);
            if (this.state == 1) {
                this.opening_ = false;
                this.listeners.forEach(function (l) { return l.opened(); });
            }
            this.listeners.forEach(function (l) { return l.moved(_this.state); });
        }
        else if (this.closing) {
            this.state_ = Math.max(0, this.state - dt * this.speed);
            if (this.state == 0) {
                this.closing_ = false;
                this.listeners.forEach(function (l) { return l.closed(); });
            }
            this.listeners.forEach(function (l) { return l.moved(_this.state); });
        }
    };
    return Door;
}(BreakableModel));
/// <reference path="./Model.ts" />
var Elevator = /** @class */ (function (_super) {
    __extends(Elevator, _super);
    function Elevator(shaft, capacity_) {
        var _this = _super.call(this) || this;
        _this.shaft = shaft;
        _this.capacity_ = capacity_;
        _this.direction = null;
        _this.stopRequested = false;
        _this.door_ = new Door();
        _this.position_ = 0;
        _this.velocity = 0;
        _this.lights = new Array(_this.shaft.building.floors.length);
        _this.people = new Array(_this.capacity);
        _this.peopleCount = 0;
        return _this;
    }
    Elevator.prototype.lightState = function (floor) {
        if (floor > 0 && floor < this.lights.length)
            return this.lights[Math.floor(floor)];
        else
            return false;
    };
    Object.defineProperty(Elevator.prototype, "capacity", {
        get: function () {
            return this.capacity_;
        },
        enumerable: true,
        configurable: true
    });
    Elevator.prototype.enter = function (person) {
        if (this.peopleCount < this.capacity) {
            var i_1 = this.people.findIndex(function (x) { return !x; });
            this.people[i_1] = true;
            this.peopleCount += 1;
            this.listeners.forEach(function (l) { return l.personEntered(person, i_1); });
            return i_1;
        }
        else {
            return null;
        }
    };
    Elevator.prototype.leave = function (i) {
        this.people[i] = false;
        this.peopleCount -= 1;
    };
    Object.defineProperty(Elevator.prototype, "broken", {
        get: function () {
            return this.broken_;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(Elevator.prototype, "door", {
        get: function () {
            return this.door_;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(Elevator.prototype, "position", {
        get: function () {
            return this.position_;
        },
        enumerable: true,
        configurable: true
    });
    Elevator.prototype.move = function (direction) {
        if (!this.broken) {
            if (this.direction != null) {
                this.break("requested movement before elevator stopped");
            }
            else if (this.door.closing) {
                this.break("requested to move while door was closing");
            }
            else if (this.door.opening) {
                this.break("requested to move while door was opening");
            }
            else if (this.door.state != 0) {
                this.break("requested movement while door was not closed");
            }
            else {
                this.direction = direction;
            }
        }
    };
    Elevator.prototype.stop = function () {
        if (!this.broken) {
            if (this.stopRequested) {
                this.break("requested to stop while already stopping");
            }
            if (this.direction == null) {
                this.break("requested to stop when elevator was already stopped");
            }
            else {
                this.stopRequested = true;
            }
        }
    };
    Elevator.prototype.openDoor = function () {
        if (!this.broken) {
            if (this.direction != null) {
                this.break("requested to open door while elevator was moving");
            }
            else {
                this.door.open();
            }
        }
    };
    Elevator.prototype.closeDoor = function () {
        if (!this.broken) {
            if (this.direction != null) {
                this.break("requested to close door while elevator was moved");
            }
            else {
                this.door.close();
            }
        }
    };
    Elevator.prototype.setLight = function (floor, on) {
        if (this.lights[floor] != on) {
            this.lights[floor] = on;
            this.listeners.forEach(function (l) { return l.lightStateChanged(floor); });
        }
    };
    Elevator.prototype.pressButton = function (floor) {
        this.listeners.forEach(function (l) { return l.buttonPressed(floor); });
    };
    Elevator.prototype.progress = function (dt) {
        if (!this.broken) {
            this.door.progress(dt);
            var p_1 = this.position;
            if (this.direction == Direction.Up) {
                p_1 += dt;
                if (this.stopRequested && Math.floor(p_1) != Math.floor(this.position)) {
                    p_1 = Math.floor(p_1);
                    this.direction = null;
                    this.stopRequested = false;
                    this.listeners.forEach(function (l) { return l.stopped(); });
                }
            }
            else if (this.direction == Direction.Down) {
                p_1 -= dt;
                if (this.stopRequested && Math.floor(p_1) != Math.floor(this.position)) {
                    p_1 = Math.floor(this.position);
                    this.direction = null;
                    this.stopRequested = false;
                    this.listeners.forEach(function (l) { return l.stopped(); });
                }
            }
            if (Math.round(this.position) != Math.round(p_1)) {
                this.listeners.forEach(function (l) { return l.floorPassed(); });
            }
            if (p_1 != this.position_)
                this.listeners.forEach(function (l) { return l.moved(p_1); });
            this.position_ = p_1;
            if (this.position > this.shaft.building.floors.length - 0.5)
                this.break("elevator moved passed seiling");
            if (this.position < 0)
                this.break("elevator moved below basement");
        }
        else {
            var p_2 = this.position_;
            if (p_2 != -0.5 || this.velocity != 0) {
                this.velocity -= dt * 9.81;
                p_2 += this.velocity * dt;
                if (this.position < -0.5) {
                    p_2 = -0.5;
                    this.velocity = -this.velocity * 0.8;
                    if (this.velocity < 1) {
                        this.velocity = 0;
                    }
                }
                if (p_2 != this.position_)
                    this.listeners.forEach(function (l) { return l.moved(p_2); });
                this.position_ = p_2;
            }
        }
    };
    return Elevator;
}(BreakableModel));
var ElevatorView = /** @class */ (function () {
    function ElevatorView(model, svg, options) {
        var _this = this;
        this.model = model;
        this.svg = svg;
        this.options = options;
        this.group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
        var rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
        var buttons = [];
        var group = this.group;
        this.group.appendChild(rect);
        var doorLeft = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
        var doorRight = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
        this.group.appendChild(doorLeft);
        this.group.appendChild(doorRight);
        this.group.classList.add("elevator");
        rect.width.baseVal.value = options.elevatorWidth;
        rect.height.baseVal.value = options.elevatorHeight;
        doorLeft.width.baseVal.value = options.elevatorWidth / 2;
        doorLeft.height.baseVal.value = options.elevatorHeight;
        doorRight.x.baseVal.value = options.elevatorWidth / 2;
        doorRight.width.baseVal.value = options.elevatorWidth / 2;
        doorRight.height.baseVal.value = options.elevatorHeight;
        doorLeft.classList.add('door');
        doorRight.classList.add('door');
        var position = svg.createSVGTransform();
        var x = model.shaft.id * (options.elevatorWidth + options.elevatorGap);
        var base = model.shaft.building.floors.length * options.floorHeight - options.elevatorHeight;
        position.setTranslate(x, base);
        this.group.transform.baseVal.initialize(position);
        this.model.shaft.building.floors.forEach(function (floor) {
            var button = document.createElementNS('http://www.w3.org/2000/svg', 'text');
            button.classList.add('elevator-button');
            button.textContent = floor.id.toString();
            _this.group.appendChild(button);
            var d = Math.min(options.elevatorWidth / 8, options.elevatorHeight / (floor.building.floorCount + 2));
            button.style.fontSize = d.toString();
            var pos = svg.createSVGTransform();
            pos.setTranslate(d, (model.shaft.building.floorCount + 2) * d - (floor.id + 1) * d);
            button.transform.baseVal.initialize(pos);
            buttons.push(button);
        });
        function pos(i) {
            var r = Math.ceil(Math.sqrt(model.capacity));
            var dx = options.elevatorWidth / (r + 1);
            var dy = options.elevatorHeight / (r + 1);
            return {
                x: dx + dx * Math.floor(i / r),
                y: dy + dy * (i % r)
            };
        }
        model.door.listen({
            opened: function () { },
            closed: function () { },
            moved: function (position) {
                var w = (1 - position) * options.elevatorWidth / 2;
                doorLeft.width.baseVal.value = w;
                doorRight.width.baseVal.value = w;
                doorRight.x.baseVal.value = options.elevatorWidth - w;
            },
            broken: function (reason) {
                doorLeft.classList.add('broken');
                doorRight.classList.add('broken');
            }
        });
        this.model.listen({
            floorPassed: function () { },
            stopped: function () { },
            buttonPressed: function (floor) { },
            broken: function (reason) { },
            personEntered: function (person, index) {
                if (person.elem.parentElement) {
                    person.elem.parentElement.removeChild(person.elem);
                }
                var p = pos(index);
                var po = svg.createSVGTransform();
                po.setTranslate(p.x, p.y);
                person.elem.transform.baseVal.initialize(po);
                group.appendChild(person.elem);
            },
            moved: function (p) {
                var y = base - p * options.floorHeight;
                position.setTranslate(x, y);
                group.transform.baseVal.initialize(position);
            },
            lightStateChanged: function (floor) {
                buttons[floor].classList.toggle('on');
            }
        });
    }
    Object.defineProperty(ElevatorView.prototype, "elem", {
        get: function () {
            return this.group;
        },
        enumerable: true,
        configurable: true
    });
    return ElevatorView;
}());
/// <reference path="./Model.ts" />
var Floor = /** @class */ (function (_super) {
    __extends(Floor, _super);
    function Floor(id, building_) {
        var _this = _super.call(this) || this;
        _this.id = id;
        _this.building_ = building_;
        _this.doors_ = [];
        _this.people = new Array(0);
        _this.peopleCount = 0;
        for (var i = 0; i < building_.shaftCount; i++) {
            _this.doors_.push(new Door());
        }
        return _this;
    }
    Object.defineProperty(Floor.prototype, "building", {
        get: function () {
            return this.building_;
        },
        enumerable: true,
        configurable: true
    });
    Floor.prototype.lightState = function (direction) {
        if (direction == Direction.Up)
            return this.lightUp_;
        else
            return this.lightDown_;
    };
    Floor.prototype.door = function (shaft) {
        return this.doors_[shaft];
    };
    Floor.prototype.enter = function (person) {
        if (this.peopleCount == this.people.length) {
            this.people.push(true);
            var i_2 = this.peopleCount;
            this.peopleCount = i_2 + 1;
            this.listeners.forEach(function (l) { return l.personEntered(person, i_2); });
            return i_2;
        }
        else {
            var i_3 = this.people.findIndex(function (x) { return !x; });
            this.people[i_3] = true;
            this.peopleCount += 1;
            this.listeners.forEach(function (l) { return l.personEntered(person, i_3); });
            return i_3;
        }
    };
    Floor.prototype.leave = function (i) {
        this.people[i] = false;
        this.peopleCount -= 1;
    };
    Floor.prototype.pressButton = function (direction) {
        this.listeners.forEach(function (l) { return l.buttonPressed(direction); });
    };
    Floor.prototype.setLight = function (direction, on) {
        if (direction == Direction.Up && this.lightUp_ != on) {
            this.lightUp_ = on;
            this.listeners.forEach(function (l) { return l.lightStateChanged(direction, on); });
        }
        if (direction == Direction.Down && this.lightDown_ != on) {
            this.lightDown_ = on;
            this.listeners.forEach(function (l) { return l.lightStateChanged(direction, on); });
        }
    };
    Floor.prototype.setDoorLight = function (shaft, direction) {
        throw new Error("Method not implemented.");
    };
    Floor.prototype.progress = function (dt) {
        var _this = this;
        this.doors_.forEach(function (door, i) {
            door.progress(dt);
            if (!door.broken && door.state > 0 && _this.building.shafts[i].elevator.position != _this.id) {
                door.break("door not fully closed while elevator not behind door");
                _this.building.shafts[i].elevator.break("elevator not behind open door");
            }
        });
    };
    return Floor;
}(Model));
var FloorView = /** @class */ (function () {
    function FloorView(model, svg, options) {
        this.model = model;
        this.svg = svg;
        this.options = options;
        this.group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
        var group = this.group;
        var floorWidth = options.textWidth + options.elevatorGap +
            model.building.shafts.length * (options.elevatorGap + options.elevatorWidth);
        var floors = model.building.floors.length;
        var leftDoors = new Array(this.model.building.shafts.length);
        var rightDoors = new Array(this.model.building.shafts.length);
        var _loop_1 = function (i) {
            var door = this_1.model.door(i);
            var left = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
            var right = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
            leftDoors[i] = left;
            rightDoors[i] = right;
            left.classList.add('door');
            right.classList.add('door');
            left.height.baseVal.value = options.elevatorHeight;
            right.height.baseVal.value = options.elevatorHeight;
            left.width.baseVal.value = options.elevatorWidth / 2;
            right.width.baseVal.value = options.elevatorWidth / 2;
            left.y.baseVal.value = -options.elevatorHeight;
            right.y.baseVal.value = -options.elevatorHeight;
            left.x.baseVal.value = options.textWidth + options.elevatorGap + (options.elevatorWidth + options.elevatorGap) * i;
            right.x.baseVal.value = options.textWidth + options.elevatorGap + (options.elevatorWidth / 2) + (options.elevatorWidth + options.elevatorGap) * i;
            this_1.group.appendChild(left);
            this_1.group.appendChild(right);
            door.listen({
                opened: function () { },
                closed: function () { },
                broken: function (reason) {
                    left.classList.add('broken');
                    right.classList.add('broken');
                },
                moved: function (pos) {
                    var w = (1 - pos) * options.elevatorWidth / 2;
                    leftDoors[i].width.baseVal.value = w;
                    rightDoors[i].width.baseVal.value = w;
                    rightDoors[i].x.baseVal.value = options.textWidth + options.elevatorGap + (options.elevatorWidth - w) + (options.elevatorWidth + options.elevatorGap) * i;
                }
            });
        };
        var this_1 = this;
        for (var i = 0; i < model.building.shafts.length; i++) {
            _loop_1(i);
        }
        var separator = document.createElementNS('http://www.w3.org/2000/svg', 'line');
        separator.classList.add('floor-separator');
        var wall = document.createElementNS('http://www.w3.org/2000/svg', 'path');
        wall.classList.add('wall');
        wall.pathSegList.initialize(wall.createSVGPathSegMovetoAbs(0, 0));
        wall.pathSegList.appendItem(wall.createSVGPathSegLinetoHorizontalRel(options.textWidth + options.elevatorGap));
        for (var e = 0; e < model.building.shafts.length; e++) {
            wall.pathSegList.appendItem(wall.createSVGPathSegLinetoVerticalRel(-options.elevatorHeight));
            wall.pathSegList.appendItem(wall.createSVGPathSegLinetoHorizontalRel(options.elevatorWidth));
            wall.pathSegList.appendItem(wall.createSVGPathSegLinetoVerticalRel(options.elevatorHeight));
            wall.pathSegList.appendItem(wall.createSVGPathSegLinetoHorizontalRel(options.elevatorGap));
        }
        wall.pathSegList.appendItem(wall.createSVGPathSegLinetoVerticalRel(-options.floorHeight));
        wall.pathSegList.appendItem(wall.createSVGPathSegLinetoHorizontalRel(-floorWidth));
        wall.pathSegList.appendItem(wall.createSVGPathSegClosePath());
        this.group.appendChild(wall);
        this.group.appendChild(separator);
        separator.x1.baseVal.value = 0;
        separator.y1.baseVal.value = 0;
        separator.x2.baseVal.value = floorWidth;
        separator.y2.baseVal.value = 0;
        var label = document.createElementNS('http://www.w3.org/2000/svg', 'text');
        label.textContent = "E" + model.id;
        label.classList.add('floor-label');
        var pos = svg.createSVGTransform();
        pos.setTranslate((options.textWidth + options.elevatorGap) / 3, -options.elevatorHeight / 2);
        label.transform.baseVal.initialize(pos);
        this.group.appendChild(label);
        var up = document.createElementNS('http://www.w3.org/2000/svg', 'path');
        up.pathSegList.initialize(up.createSVGPathSegMovetoAbs((options.textWidth + options.elevatorGap) / 3, -3.3 * options.elevatorHeight / 4));
        up.pathSegList.appendItem(up.createSVGPathSegLinetoRel(-10, 10));
        up.pathSegList.appendItem(up.createSVGPathSegLinetoHorizontalRel(20));
        up.pathSegList.appendItem(up.createSVGPathSegClosePath());
        up.classList.add('button', 'up');
        this.group.appendChild(up);
        var down = document.createElementNS('http://www.w3.org/2000/svg', 'path');
        down.pathSegList.initialize(down.createSVGPathSegMovetoAbs((options.textWidth + options.elevatorGap) / 3, -options.elevatorHeight / 4));
        down.pathSegList.appendItem(down.createSVGPathSegLinetoRel(-10, -10));
        down.pathSegList.appendItem(down.createSVGPathSegLinetoHorizontalRel(20));
        down.pathSegList.appendItem(down.createSVGPathSegClosePath());
        down.classList.add('button', 'down');
        this.group.appendChild(down);
        var transform = svg.createSVGTransform();
        transform.setTranslate(0, (floors - model.id) * options.floorHeight);
        this.group.transform.baseVal.initialize(transform);
        function p(i) {
            var r = Math.floor(options.floorHeight / 10) - 1;
            var dx = 10;
            var dy = 10;
            return {
                x: dx + dx * Math.floor(i / r),
                y: dy + dy * (i % r)
            };
        }
        this.model.listen({
            buttonPressed: function (direction) { },
            personEntered: function (person, index) {
                if (person.elem.parentElement)
                    person.elem.parentElement.removeChild(person.elem);
                var t = svg.createSVGTransform();
                var po = p(index);
                t.setTranslate(options.textWidth + options.elevatorGap - po.x, -po.y);
                person.elem.transform.baseVal.initialize(t);
                group.appendChild(person.elem);
            },
            lightStateChanged: function (direction, on) {
                if (direction == Direction.Up)
                    up.classList.toggle('on');
                else
                    down.classList.toggle('on');
            }
        });
    }
    Object.defineProperty(FloorView.prototype, "elem", {
        get: function () {
            return this.group;
        },
        enumerable: true,
        configurable: true
    });
    return FloorView;
}());
function init() {
    var svg = (function () {
        var elem = document.querySelector('svg');
        if (!elem)
            throw new Error("could not find root svg element");
        return elem;
    })();
    var params = new URLSearchParams(window.location.search);
    var n = Number.parseInt(params.get('shafts') || '4') || 4; // number of elevators
    var floors = Number.parseInt(params.get('floors') || '9') || 9;
    var speed = Number.parseFloat(params.get('speed') || '1.0') || 1.0;
    var capacity = Number.parseInt(params.get('capacity') || '12') || 12;
    var socket = new WebSocket("ws://" + location.host);
    function send(event) {
        if (socket)
            socket.send(JSON.stringify(event));
        else
            console.error("socket was closed when trying to send message", event);
    }
    var options = {
        textWidth: 200,
        floorHeight: 200,
        elevatorHeight: 150,
        elevatorWidth: 100,
        elevatorGap: 50
    };
    var floorWidth = options.textWidth + options.elevatorGap + n * (options.elevatorWidth + options.elevatorGap);
    var building = new Building(floors, n);
    svg.setAttribute('viewBox', "0 0 0 0");
    svg.viewBox.baseVal.width = floorWidth;
    svg.viewBox.baseVal.height = floors * options.floorHeight;
    var elevators = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    var pos = svg.createSVGTransform();
    pos.setTranslate(options.textWidth + options.elevatorGap, 0);
    elevators.transform.baseVal.initialize(pos);
    var buildingElem = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
    buildingElem.width.baseVal.value = floorWidth;
    buildingElem.height.baseVal.value = options.floorHeight * floors;
    buildingElem.classList.add('building');
    var floorsElem = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    //let peopleElem = document.createElementNS('http://www.w3.org/2000/svg','g')
    svg.appendChild(buildingElem);
    svg.appendChild(elevators);
    svg.appendChild(floorsElem);
    //svg.appendChild(peopleElem)
    var i = 0;
    var interval = window.setInterval(function () {
        var elem = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
        elem.r.baseVal.value = 4;
        elem.classList.add('person');
        var p = new Person(building, elem, speed);
        if (i >= 250)
            window.clearInterval(interval);
        i++;
    }, 250 / speed);
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
    building.shafts.forEach(function (shaft) {
        var view = new ElevatorView(shaft.elevator, svg, options);
        elevators.appendChild(view.elem);
        shaft.elevator.listen({
            broken: function (reason) {
                send({
                    tag: "ElevatorBroken",
                    "shaft": shaft.id,
                    "reason": reason
                });
            },
            floorPassed: function () {
                send({
                    tag: "FloorPassed",
                    "shaft": shaft.id
                });
            },
            stopped: function () {
                send({
                    tag: "ElevatorStopped",
                    "shaft": shaft.id
                });
            },
            buttonPressed: function (floor) {
                send({
                    tag: "ElevatorButtonPressed",
                    "shaft": shaft.id,
                    "floor": floor
                });
            },
            personEntered: function () { },
            moved: function () { },
            lightStateChanged: function () { }
        });
        shaft.elevator.door.listen({
            opened: function () {
                send({
                    tag: "ElevatorDoorOpened",
                    "shaft": shaft.id
                });
            },
            closed: function () {
                send({
                    tag: "ElevatorDoorClosed",
                    "shaft": shaft.id
                });
            },
            broken: function (reason) {
                send({
                    tag: "ElevatorDoorBroken",
                    "shaft": shaft.id,
                    "reason": reason
                });
            },
            moved: function () { }
        });
    });
    building.floors.forEach(function (floor) {
        var view = new FloorView(floor, svg, options);
        floorsElem.appendChild(view.elem);
        floor.listen({
            buttonPressed: function (direction) {
                send({
                    tag: "FloorButtonPressed",
                    "floor": floor.id,
                    "direction": direction == Direction.Up ? "Up" : "Down"
                });
            },
            personEntered: function () { },
            lightStateChanged: function () { }
        });
        var _loop_2 = function (i_4) {
            var j = i_4;
            floor.door(j).listen({
                opened: function () {
                    send({
                        tag: "FloorDoorOpened",
                        floor: floor.id,
                        shaft: j
                    });
                },
                closed: function () {
                    send({
                        tag: "FloorDoorClosed",
                        floor: floor.id,
                        shaft: j
                    });
                },
                broken: function (message) {
                    send({
                        tag: "FloorDoorBroken",
                        "floor": floor.id,
                        "shaft": j,
                        "reason": message
                    });
                },
                moved: function () { }
            });
        };
        for (var i_4 = 0; i_4 < building.shaftCount; i_4++) {
            _loop_2(i_4);
        }
    });
    var t0 = 0;
    var base = options.floorHeight * floors - options.elevatorHeight;
    function loop(dt) {
        building.shafts.forEach(function (shaft) {
            shaft.elevator.progress(dt);
        });
        building.floors.forEach(function (floor) {
            floor.progress(dt);
        });
    }
    var renderLoop = function (t) {
        var dt = t - t0;
        t0 = t;
        loop(Math.min(0.1, speed * (dt / 1000)));
        window.requestAnimationFrame(renderLoop);
    };
    window.requestAnimationFrame(renderLoop);
    socket.addEventListener('open', function (e) {
        send({
            "tag": "Initialize",
            "floors": floors,
            "shafts": n,
            "capacity": capacity
        });
    });
    socket.addEventListener('message', function (e) {
        if (e.data != "pong") {
            var msg = JSON.parse(e.data);
            if (msg["shaft"] && (msg["shaft"] < 0 || msg["shaft"] >= building.shaftCount))
                console.error("shaft " + msg["shaft"] + " does not exist", msg);
            else if (msg["floor"] && (msg["floor"] < 0 || msg["floor"] >= building.floorCount))
                console.error("floor " + msg["floor"] + " does not exist", msg);
            else {
                switch (msg["tag"]) {
                    case "MoveElevator":
                        //(shaft: Int, direction: Direction)
                        building.shafts[msg["shaft"]].elevator.move(msg["direction"] == "Up" ? Direction.Up : Direction.Down);
                        break;
                    case "StopElevator":
                        building.shafts[msg["shaft"]].elevator.stop();
                        //(shaft: Int)
                        break;
                    case "OpenElevatorDoor":
                        //(shaft: Int)          
                        building.shafts[msg["shaft"]].elevator.openDoor();
                        break;
                    case "CloseElevatorDoor":
                        //(shaft: Int)          
                        building.shafts[msg["shaft"]].elevator.closeDoor();
                        break;
                    case "OpenFloorDoor":
                        building.floors[msg["floor"]].door(msg["shaft"]).open();
                        break;
                    case "CloseFloorDoor":
                        building.floors[msg["floor"]].door(msg["shaft"]).close();
                        break;
                    case "SetElevatorButtonLight":
                        building.shafts[msg["shaft"]].elevator.setLight(msg["floor"], msg["on"]);
                        break;
                    case "SetFloorButtonLight":
                        //(floor: Int, direction: Direction, on: Boolean)
                        building.floors[msg["floor"]].setLight(msg["direction"] == "Up" ? Direction.Up : Direction.Down, msg["on"]);
                        break;
                    default:
                        break;
                }
            }
        }
    });
}
document.addEventListener("DOMContentLoaded", init, {
    once: true
});
var Person = /** @class */ (function () {
    function Person(building, elem, speedFactor) {
        this.building = building;
        this.elem = elem;
        this.speedFactor = speedFactor;
        this.waitingSince = 0;
        this.position_ = building.floors[0];
        this.index_ = this.position_.enter(this);
        this.act(0);
        this.id_ = Person.idCounter++;
    }
    Object.defineProperty(Person.prototype, "id", {
        get: function () {
            return this.id_;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(Person.prototype, "waitTime", {
        get: function () {
            return Math.min(1, (Date.now() - this.waitingSince) / (60000 / this.speedFactor));
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(Person.prototype, "position", {
        get: function () {
            return this.position_;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(Person.prototype, "index", {
        get: function () {
            return this.index_;
        },
        enumerable: true,
        configurable: true
    });
    Person.prototype.act = function (delay) {
        var _this = this;
        window.setTimeout(function () {
            if (!_this.destination) {
                _this.waitingSince = Date.now();
                _this.destination = Math.floor(Math.random() * _this.building.floors.length);
                _this.elem.style.fill = "#0f0";
                _this.act(2000);
            }
            else if (_this.position instanceof Floor) {
                var r = Math.min(255, 512 * _this.waitTime);
                var g = Math.min(255, 512 - 512 * _this.waitTime);
                _this.elem.style.fill = "rgb(" + r + "," + g + ",0)";
                if (_this.destination == _this.position.id) {
                    _this.destination = null;
                }
                else {
                    var f_1 = _this.position;
                    _this.building.shafts.forEach(function (shaft) {
                        if (!(_this.position_ instanceof Elevator) && shaft.elevator.position == f_1.id && shaft.elevator.door.state > 0.8 && f_1.door(shaft.id).state > 0.8) {
                            var i = shaft.elevator.enter(_this);
                            if (i != null) {
                                _this.waitingSince = Date.now();
                                _this.destination = Math.floor(Math.random() * _this.building.floors.length);
                                _this.elem.style.fill = "#0f0";
                                _this.position_.leave(_this.index);
                                _this.position_ = shaft.elevator;
                                _this.index_ = i;
                            }
                        }
                    });
                    var dir = _this.destination < _this.position.id ? Direction.Down : Direction.Up;
                    if (!_this.position.lightState(dir))
                        _this.position.pressButton(dir);
                }
                _this.act(500);
            }
            else {
                var r = Math.min(255, 512 * _this.waitTime);
                var g = Math.min(255, 512 - 512 * _this.waitTime);
                _this.elem.style.fill = "rgb(" + r + "," + g + ",0)";
                if (!_this.position.lightState(_this.destination))
                    _this.position.pressButton(_this.destination);
                if (_this.position.door.state > 0.5 && _this.position.position == _this.destination) {
                    _this.position.leave(_this.index);
                    _this.position_ = _this.building.floors[_this.destination];
                    _this.index_ = _this.position_.enter(_this);
                    _this.destination = null;
                    _this.elem.style.fill = "#fff";
                }
                _this.act(500);
            }
        }, (delay + Math.random() * 500) / this.speedFactor);
    };
    Person.idCounter = 0;
    return Person;
}());
var Shaft = /** @class */ (function () {
    function Shaft(id, building, capacity) {
        this.id = id;
        this.building = building;
        this.capacity = capacity;
        this.elevator_ = new Elevator(this, capacity);
    }
    Object.defineProperty(Shaft.prototype, "elevator", {
        get: function () {
            return this.elevator_;
        },
        enumerable: true,
        configurable: true
    });
    return Shaft;
}());
//# sourceMappingURL=elevator.js.map