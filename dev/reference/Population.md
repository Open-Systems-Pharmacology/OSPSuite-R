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
- [`ospsuite::DotNetWrapper$initialize()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.html#method-initialize)

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
