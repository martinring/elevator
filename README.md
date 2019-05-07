# Elevator

In this exercise you need to implement an elevator controller in either Haskell or Scala. The folder `gui` contains a simulation running in an svg which communicates with your implementation. You do not need to touch this part. Choose your language and then consult either.

- `haskell/README.md` or
- `scala/README.md`

for instructions on running.

## Configuring the simulation

When you started your implementation you can access it on `localhost:3000/elevator.svg`. There are three available configuration variables which may be changed by passing query parameters to the url.

- `floors`: an integer indicating how many floors the building should have (Default: 9)
- `shafts`: an integer indicating how many elevator shafts there should be (Default: 4)
- `speed`: a floating point number which is a multiplier on the simulation time. Higher values = faster simulation. (Default: 1.0)

Example: `localhost:3000/elevator.svg?floor=20&speed=1.5` creates a building with 20 floors and runs the simulation by factor 1.5 faster.