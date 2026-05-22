# OutputSchema

Output schema associated with a given simulation

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
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

### `OutputSchema$clear()`

Clears all intervals and time points

#### Usage

    OutputSchema$clear()

------------------------------------------------------------------------

### `OutputSchema$addInterval()`

Adds an interval to the schema

#### Usage

    OutputSchema$addInterval(interval)

#### Arguments

- `interval`:

  Interval to add

------------------------------------------------------------------------

### `OutputSchema$removeInterval()`

Removes the interval from the schema

#### Usage

    OutputSchema$removeInterval(interval)

#### Arguments

- `interval`:

  Interval to remove

------------------------------------------------------------------------

### `OutputSchema$addTimePoints()`

Adds the time points to the schema. Note that time points and intervals
exists concurrently. Use time points only if you need to ensure that
specific time are used.

#### Usage

    OutputSchema$addTimePoints(timePoints)

#### Arguments

- `timePoints`:

  Time points to add to the schema

------------------------------------------------------------------------

### `OutputSchema$print()`

Print the object to the console

#### Usage

    OutputSchema$print(...)

#### Arguments

- `...`:

  Rest arguments.
