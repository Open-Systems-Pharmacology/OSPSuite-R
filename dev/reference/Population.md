# Population

List of individuals used in a population simulation

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\> `Population`

## Active bindings

- `count`:

  the number of individual in the population

- `allCovariateNames`:

  the names of all covariates defined in the population

- `allParameterPaths`:

  the paths of all parameters defined in the population

- `allIndividualIds`:

  Ids of individuals defined in the population

## Methods

### Public methods

- [`Population$has()`](#method-Population-has)

- [`Population$setParameterValues()`](#method-Population-setParameterValues)

- [`Population$getParameterValues()`](#method-Population-getParameterValues)

- [`Population$getCovariateValues()`](#method-Population-getCovariateValues)

- [`Population$getCovariateValue()`](#method-Population-getCovariateValue)

- [`Population$getParameterValuesForIndividual()`](#method-Population-getParameterValuesForIndividual)

- [`Population$remove()`](#method-Population-remove)

- [`Population$print()`](#method-Population-print)

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

### Method `has()`

Returns `TRUE` if the population has variability defined for
`parameterOrPath` otherwise `FALSE`

#### Usage

    Population$has(parameterOrPath)

#### Arguments

- `parameterOrPath`:

  Parameter instance of parameter path

------------------------------------------------------------------------

### Method [`setParameterValues()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/setParameterValues.md)

Updates or adds the variability values in the population for
`parameterOrPath`.

#### Usage

    Population$setParameterValues(parameterOrPath, values)

#### Arguments

- `parameterOrPath`:

  Parameter instance of parameter path. If an entry already exists for
  this parameter by path, its values be overwritten, otherwise it will
  be created.

- `values`:

  double vector containing the value to set for the `parameterOrPath`

------------------------------------------------------------------------

### Method `getParameterValues()`

Returns the variability values defined in the population for
`parameterOrPath`

#### Usage

    Population$getParameterValues(parameterOrPath)

#### Arguments

- `parameterOrPath`:

  Parameter instance of parameter path

------------------------------------------------------------------------

### Method `getCovariateValues()`

Returns the values defined in the population for the covariate named
`covariateName`

#### Usage

    Population$getCovariateValues(covariateName)

#### Arguments

- `covariateName`:

  Name of covariate for which values should be retrieved

------------------------------------------------------------------------

### Method `getCovariateValue()`

Returns the values defined in the population for the covariate named
`covariateName` and individual with id `individualId`

#### Usage

    Population$getCovariateValue(covariateName, individualId)

#### Arguments

- `covariateName`:

  Name of covariate for which values should be retrieved

- `individualId`:

  Id of individual for which the value for covariate `covariateName`
  should be retrieved

------------------------------------------------------------------------

### Method `getParameterValuesForIndividual()`

Returns all values defined in the population the individual with id
`individualId`

#### Usage

    Population$getParameterValuesForIndividual(individualId)

#### Arguments

- `individualId`:

  Id of individual for which all values should be returned

------------------------------------------------------------------------

### Method [`remove()`](https://rdrr.io/r/base/rm.html)

Removes the value of a parameter by path

#### Usage

    Population$remove(parameterPath)

#### Arguments

- `parameterPath`:

  Path of the parameter values to remove

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    Population$print(...)

#### Arguments

- `...`:

  Rest arguments.
