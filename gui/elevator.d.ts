declare class Building {
    floorCount: number;
    shaftCount: number;
    private floors_;
    private shafts_;
    constructor(floorCount: number, shaftCount: number);
    readonly floors: Floor[];
    readonly shafts: Shaft[];
}
declare enum Direction {
    Up = 0,
    Down = 1
}
declare abstract class Model<Events> {
    protected listeners: Array<Events>;
    listen(listener: Events): void;
    abstract progress(dt: number): void;
}
declare abstract class BreakableModel<Events extends {
    broken(reason: string): void;
}> extends Model<Events> {
    protected broken_: boolean;
    readonly broken: boolean;
    break(reason: string): void;
}
declare class Door extends BreakableModel<DoorEvents> {
    private speed;
    private state_;
    private opening_;
    private closing_;
    constructor();
    readonly state: number;
    readonly broken: boolean;
    readonly closing: boolean;
    readonly opening: boolean;
    open(): void;
    close(): void;
    progress(dt: number): void;
}
interface DoorEvents {
    opened(): void;
    closed(): void;
    moved(position: number): void;
    broken(reason: string): void;
}
declare class Elevator extends BreakableModel<ElevatorEvents> implements ElevatorControl {
    shaft: Shaft;
    private capacity_;
    constructor(shaft: Shaft, capacity_: number);
    private direction;
    private stopRequested;
    private door_;
    private position_;
    private velocity;
    private lights;
    lightState(floor: number): boolean;
    readonly capacity: number;
    private people;
    private peopleCount;
    enter(person: Person): number | null;
    leave(i: number): void;
    readonly broken: boolean;
    readonly door: Door;
    readonly position: number;
    move(direction: Direction): void;
    stop(): void;
    openDoor(): void;
    closeDoor(): void;
    setLight(floor: number, on: boolean): void;
    pressButton(floor: number): void;
    progress(dt: number): void;
}
interface ElevatorControl {
    move(direction: Direction): void;
    stop(): void;
    openDoor(): void;
    closeDoor(): void;
    setLight(floor: number, on: boolean): void;
}
interface ElevatorEvents {
    floorPassed(): void;
    stopped(): void;
    buttonPressed(floor: number): void;
    broken(reason: string): void;
    personEntered(person: Person, index: number): void;
    moved(position: number): void;
    lightStateChanged(floor: number): void;
}
declare class ElevatorView implements View {
    private model;
    private svg;
    private options;
    private group;
    readonly elem: SVGElement;
    constructor(model: Elevator, svg: SVGSVGElement, options: ViewOptions);
}
declare class Floor extends Model<FloorEvents> implements FloorControl {
    id: number;
    private building_;
    constructor(id: number, building_: Building);
    readonly building: Building;
    private doors_;
    private lightUp_;
    private lightDown_;
    lightState(direction: Direction): boolean;
    door(shaft: number): Door;
    private people;
    private peopleCount;
    enter(person: Person): number;
    leave(i: number): void;
    pressButton(direction: Direction): void;
    setLight(direction: Direction, on: boolean): void;
    setDoorLight(shaft: number, direction: Direction | null): void;
    progress(dt: number): void;
}
interface FloorControl {
    setLight(direction: Direction, on: boolean): void;
    setDoorLight(shaft: number, direction: Direction | null): void;
}
interface FloorEvents {
    buttonPressed(direction: Direction): void;
    personEntered(person: Person, index: number): void;
    lightStateChanged(direction: Direction, on: Boolean): void;
}
declare class FloorView implements View {
    private model;
    private svg;
    private options;
    private group;
    readonly elem: SVGElement;
    constructor(model: Floor, svg: SVGSVGElement, options: ViewOptions);
}
declare function init(): void;
declare class Person {
    private building;
    elem: SVGGraphicsElement;
    private speedFactor;
    private position_;
    index_: number;
    private destination;
    private id_;
    private static idCounter;
    constructor(building: Building, elem: SVGGraphicsElement, speedFactor: number);
    private waitingSince;
    readonly id: number;
    readonly waitTime: number;
    readonly position: Floor | Elevator;
    readonly index: number;
    act(delay: number): void;
}
declare class Shaft {
    id: number;
    building: Building;
    private capacity;
    private elevator_;
    constructor(id: number, building: Building, capacity: number);
    readonly elevator: Elevator;
}
interface View {
    elem: SVGElement;
}
interface ViewOptions {
    elevatorWidth: number;
    elevatorHeight: number;
    elevatorGap: number;
    floorHeight: number;
    textWidth: number;
}
