# Interval

Simulation Interval (typically associated with an instance of
`OutputSchema`)

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DotNetWrapper.md)
-\>
[`ospsuite::ObjectBase`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/ObjectBase.md)
-\>
[`ospsuite::Entity`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/Entity.md)
-\>
[`ospsuite::Container`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/Container.md)
-\> `Interval`

## Active bindings

- `startTime`:

  Start time of interval (instance of `Parameter`)

- `endTime`:

  End time of interval (instance of `Parameter`)

- `resolution`:

  Resolution of interval in pts/min (instance of `Parameter`)

- `name`:

  Name of the interval

## Methods

### Public methods

- [`Interval$print()`](#method-Interval-print)

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
- [`ospsuite::DotNetWrapper$initialize()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DotNetWrapper.html#method-initialize)

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    Interval$print(...)

#### Arguments

- `...`:

  Rest arguments.
