# DataRepository

An object typically holding observed data

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DotNetWrapper.md)
-\> `DataRepository`

## Active bindings

- `name`:

  The name of the object.

- `baseGrid`:

  Returns the base column for the data repository (typically time
  column).

- `columns`:

  Returns all columns (including baseGrid) defined in the data
  repository.

- `allButBaseGrid`:

  Returns all columns excluding baseGrid defined on the data repository.

- `metaData`:

  Returns a named list of meta data defined for the data repository.
  where the name is the name of the metaData and the value is the meta
  data value.

## Methods

### Public methods

- [`DataRepository$addColumn()`](#method-DataRepository-addColumn)

- [`DataRepository$new()`](#method-DataRepository-new)

- [`DataRepository$print()`](#method-DataRepository-print)

- [`DataRepository$addMetaData()`](#method-DataRepository-addMetaData)

- [`DataRepository$removeMetaData()`](#method-DataRepository-removeMetaData)

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

------------------------------------------------------------------------

### Method `addColumn()`

Adds a column to the data repository

#### Usage

    DataRepository$addColumn(column)

#### Arguments

- `column`:

  Column to add

------------------------------------------------------------------------

### Method `new()`

Initialize a new instance of the class

#### Usage

    DataRepository$new(netObj = NULL)

#### Arguments

- `netObj`:

  Optional `NetObject` to the pointer of the underlying
  `DataRepository`. If it is not provided, a new instance will be
  created

#### Returns

A new `DataRepository` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    DataRepository$print(...)

#### Arguments

- `...`:

  Rest arguments.

------------------------------------------------------------------------

### Method `addMetaData()`

Adds a new entry to meta data list or changes its value if the name is
already present.

#### Usage

    DataRepository$addMetaData(name, value)

#### Arguments

- `name`:

  Name of new meta data list entry

- `value`:

  Value of new meta data list entry

------------------------------------------------------------------------

### Method `removeMetaData()`

Removes the meta data entry in the list if one is defined with this name

#### Usage

    DataRepository$removeMetaData(name)

#### Arguments

- `name`:

  Name of meta data entry to delete
