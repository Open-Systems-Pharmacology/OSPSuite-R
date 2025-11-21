# DataColumn

One column defined in a `DataRepository`

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DotNetWrapper.md)
-\> `DataColumn`

## Active bindings

- `values`:

  Returns the values defined in the column

- `name`:

  Returns the name of the column (Read-Only)

- `unit`:

  The base unit in which the values are defined (Read-Only)

- `displayUnit`:

  The unit in which the values should be displayed

- `dimension`:

  The dimension of the values

- `molWeight`:

  Molecular weight of associated observed data in internal unit In no
  molecular weight is defined, the value is `NULL`

- `LLOQ`:

  Lower Limit Of Quantification. In no LLOQ is defined, the value is
  `NULL`

## Methods

### Public methods

- [`DataColumn$print()`](#method-DataColumn-print)

Inherited methods

- [`rSharp::NetObject$.printClass()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-.printClass)
- [`rSharp::NetObject$.printLine()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-.printLine)
- [`rSharp::NetObject$call()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-call)
- [`rSharp::NetObject$get()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-get)
- [`rSharp::NetObject$getFields()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getFields)
- [`rSharp::NetObject$getMemberSignature()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getMemberSignature)
- [`rSharp::NetObject$getMethods()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getMethods)
- [`rSharp::NetObject$getProperties()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getProperties)
- [`rSharp::NetObject$getStaticFields()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getStaticFields)
- [`rSharp::NetObject$getStaticMethods()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getStaticMethods)
- [`rSharp::NetObject$getStaticProperties()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getStaticProperties)
- [`rSharp::NetObject$set()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-set)
- [`ospsuite::DotNetWrapper$initialize()`](https://www.open-systems-pharmacology.org/ospsuite/html/DotNetWrapper.html#method-DotNetWrapper-initialize)

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    DataColumn$print(...)

#### Arguments

- `...`:

  Rest arguments.
