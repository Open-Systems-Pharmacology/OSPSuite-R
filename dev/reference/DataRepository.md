# DataRepository

An object typically holding observed data

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
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
