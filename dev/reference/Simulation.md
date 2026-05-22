# Simulation

An OSPSuite simulation

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\>
[`ObjectBase`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ObjectBase.md)
-\> `Simulation`

## Active bindings

- `root`:

  Root container of the simulation (read-only)

- `path`:

  Path of the root container of the simulation (read-only)

- `solver`:

  SimulationSolver object for the simulation (read-only)

- `outputSchema`:

  outputSchema object for the simulation (read-only)

- `outputSelections`:

  outputSelections object for the simulation (read-only)

- `sourceFile`:

  Path to the file the simulation was loaded from (read-only)

- `name`:

  Name of the simulation

- `configuration`:

  An object of the type `SimulationConfiguration`, describing the
  modules used for the simulation, selected Parameter Values (PV) and
  Initial Conditions (IC).

## Methods

### Public methods

- [`Simulation$new()`](#method-Simulation-initialize)

- [`Simulation$allEndogenousStationaryMoleculeNames()`](#method-Simulation-allEndogenousStationaryMoleculeNames)

- [`Simulation$allXenobioticFloatingMoleculeNames()`](#method-Simulation-allXenobioticFloatingMoleculeNames)

- [`Simulation$allStationaryMoleculeNames()`](#method-Simulation-allStationaryMoleculeNames)

- [`Simulation$allFloatingMoleculeNames()`](#method-Simulation-allFloatingMoleculeNames)

- [`Simulation$molWeightFor()`](#method-Simulation-molWeightFor)

- [`Simulation$calculationMethodFor()`](#method-Simulation-calculationMethodFor)

- [`Simulation$allApplicationsFor()`](#method-Simulation-allApplicationsFor)

- [`Simulation$print()`](#method-Simulation-print)

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

### `Simulation$new()`

Initialize a new instance of the class

#### Usage

    Simulation$new(netObject, sourceFile = NULL)

#### Arguments

- `netObject`:

  Reference to `NetObject` .NET simulation object

- `sourceFile`:

  (Optional) File used to load the simulation

#### Returns

A new `Simulation` object.

------------------------------------------------------------------------

### `Simulation$allEndogenousStationaryMoleculeNames()`

Returns the name of all endogenous stationary molecules defined in the
simulation. (e.g. with the flag IsStationary = TRUE) This is a typically
a molecule that is individual specific such as en Enzyme, Protein,
Transporter, FcRn etc.

#### Usage

    Simulation$allEndogenousStationaryMoleculeNames()

------------------------------------------------------------------------

### `Simulation$allXenobioticFloatingMoleculeNames()`

Returns the name of all xenobiotic floating molecules defined in the
simulation. (e.g. with the flag IsStationary = FALSE) This is typically
a molecule that is being explicitly simulated such as Compound,
Inhibitor, DrugComplex.

#### Usage

    Simulation$allXenobioticFloatingMoleculeNames()

------------------------------------------------------------------------

### `Simulation$allStationaryMoleculeNames()`

Returns the name of all stationary molecules defined in the simulation.
(e.g. with the flag IsStationary = TRUE)

#### Usage

    Simulation$allStationaryMoleculeNames()

------------------------------------------------------------------------

### `Simulation$allFloatingMoleculeNames()`

Returns the name of all floating molecules defined in the simulation.
(e.g. with the flag IsStationary = FALSE)

#### Usage

    Simulation$allFloatingMoleculeNames()

------------------------------------------------------------------------

### `Simulation$molWeightFor()`

Returns the mol weight value (in core unit) associated to the quantity
with given path or NA if not found

#### Usage

    Simulation$molWeightFor(quantityPath)

#### Arguments

- `quantityPath`:

  Path of quantity used to retrieve the molecular weight

------------------------------------------------------------------------

### `Simulation$calculationMethodFor()`

Returns the calculation method name used for the given molecule and
category, or `NULL` if no override is set.

#### Usage

    Simulation$calculationMethodFor(moleculeName, category)

#### Arguments

- `moleculeName`:

  Name of the molecule.

- `category`:

  One of the `CalculationMethodCategories` enum values (e.g.
  `CalculationMethodCategories$PartitionCoefficient`).

------------------------------------------------------------------------

### `Simulation$allApplicationsFor()`

Returns the applications ordered by start time associated to the
quantity with path `quantityPath` or an empty list if not found

#### Usage

    Simulation$allApplicationsFor(quantityPath)

#### Arguments

- `quantityPath`:

  Path of quantity used to retrieve the applications (e.g. applications
  resulting in this quantity being applied)

------------------------------------------------------------------------

### `Simulation$print()`

Print the object to the console

#### Usage

    Simulation$print(printClassProperties = FALSE, ...)

#### Arguments

- `printClassProperties`:

  Logical, whether to print class properties (default: `FALSE`). If
  `TRUE`, calls first the `print` method of the parent class. Useful for
  debugging.

- `...`:

  Rest arguments.
