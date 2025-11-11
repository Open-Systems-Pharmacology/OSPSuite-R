# OutputSchema

Output schema associated with a given simulation

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\> `OutputSchema`

## Active bindings

- `intervals`:

  All intervals defined in the schema (Read-Only)

- `timePoints`:

  All single time points defined in the schema (Read-Only)

- `endTime`:

  Returns the end time of the simulation in kernel unit (Read-Only)

## Methods

### Public methods

- [`OutputSchema$clear()`](#method-OutputSchema-clear)

- [`OutputSchema$addInterval()`](#method-OutputSchema-addInterval)

- [`OutputSchema$removeInterval()`](#method-OutputSchema-removeInterval)

- [`OutputSchema$addTimePoints()`](#method-OutputSchema-addTimePoints)

- [`OutputSchema$print()`](#method-OutputSchema-print)

Inherited methods

- [`rSharp::NetObject$.printClass()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-.printClass)
- [`rSharp::NetObject$.printLine()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-.printLine)
- [`rSharp::NetObject$call()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-call)
- [`rSharp::NetObject$get()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-get)
- [`rSharp::NetObject$getFields()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-getFields)
- [`rSharp::NetObject$getMemberSignature()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-getMemberSignature)
- [`rSharp::NetObject$getMethods()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-getMethods)
- [`rSharp::NetObject$getProperties()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-getProperties)
- [`rSharp::NetObject$getStaticFields()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-getStaticFields)
- [`rSharp::NetObject$getStaticMethods()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-getStaticMethods)
- [`rSharp::NetObject$getStaticProperties()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-getStaticProperties)
- [`rSharp::NetObject$set()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-set)
- [`ospsuite::DotNetWrapper$initialize()`](https://www.open-systems-pharmacology.org/OSPSuite-R/ospsuite/html/DotNetWrapper.html#method-DotNetWrapper-initialize)

------------------------------------------------------------------------

### Method `clear()`

Clears all intervals and time points

#### Usage

    OutputSchema$clear()

------------------------------------------------------------------------

### Method `addInterval()`

Adds an interval to the schema

#### Usage

    OutputSchema$addInterval(interval)

#### Arguments

- `interval`:

  Interval to add

------------------------------------------------------------------------

### Method `removeInterval()`

Removes the interval from the schema

#### Usage

    OutputSchema$removeInterval(interval)

#### Arguments

- `interval`:

  Interval to remove

------------------------------------------------------------------------

### Method `addTimePoints()`

Adds the time points to the schema. Note that time points and intervals
exists concurrently. Use time points only if you need to ensure that
specific time are used.

#### Usage

    OutputSchema$addTimePoints(timePoints)

#### Arguments

- `timePoints`:

  Time points to add to the schema

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    OutputSchema$print(...)

#### Arguments

- `...`:

  Rest arguments.
