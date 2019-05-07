interface DoorEvents {
  opened(): void
  closed(): void
  moved(position: number): void
  broken(reason: string): void
}