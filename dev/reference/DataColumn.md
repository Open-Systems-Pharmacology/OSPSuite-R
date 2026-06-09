# DataColumn

One column defined in a `DataRepository`

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
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

### `DataColumn$print()`

Print the object to the console

#### Usage

    DataColumn$print(...)

#### Arguments

- `...`:

  Rest arguments.
