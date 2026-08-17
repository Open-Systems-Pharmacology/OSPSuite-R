# SolverSettings

Solver settings associated with a given simulation

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\> `SolverSettings`

## Active bindings

- `useJacobian`:

  Use of Jacobian matrix during calculations

- `h0`:

  Initial time step size

- `hMin`:

  Minimum absolute value of step size allowed

- `hMax`:

  Maximum absolute value of step size allowed

- `mxStep`:

  Maximum number of internal steps to be taken by the solver in its
  attempt to reach tout

- `relTol`:

  Relative tolerance of unknowns

- `absTol`:

  Absolute tolerance of unknowns

- `checkForNegativeValues`:

  Should the solver check for negative values

## Methods

### Public methods

- [`SolverSettings$print()`](#method-SolverSettings-print)

Inherited methods

- [`rSharp::NetObject$.printClass()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-.printClass)
- [`rSharp::NetObject$.printLine()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-.printLine)
- [`rSharp::NetObject$call()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-call)
- [`rSharp::NetObject$get()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-get)
- [`rSharp::NetObject$getFields()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getFields)
- [`rSharp::NetObject$getMemberSignature()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getMemberSignature)
- [`rSharp::NetObject$getMethods()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getMethods)
- [`rSharp::NetObject$getProperties()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getProperties)
- [`rSharp::NetObject$getStaticFields()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getStaticFields)
- [`rSharp::NetObject$getStaticMethods()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getStaticMethods)
- [`rSharp::NetObject$getStaticProperties()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getStaticProperties)
- [`rSharp::NetObject$set()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-set)
- [`DotNetWrapper$initialize()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.html#method-initialize)

------------------------------------------------------------------------

### `SolverSettings$print()`

Print the object to the console

#### Usage

    SolverSettings$print(...)

#### Arguments

- `...`:

  Rest arguments.
