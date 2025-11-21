# Simulation

An OSPSuite simulation

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DotNetWrapper.md)
-\>
[`ospsuite::ObjectBase`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/ObjectBase.md)
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

## Methods

### Public methods

- [`Simulation$new()`](#method-Simulation-new)

- [`Simulation$allEndogenousStationaryMoleculeNames()`](#method-Simulation-allEndogenousStationaryMoleculeNames)

- [`Simulation$allXenobioticFloatingMoleculeNames()`](#method-Simulation-allXenobioticFloatingMoleculeNames)

- [`Simulation$allStationaryMoleculeNames()`](#method-Simulation-allStationaryMoleculeNames)

- [`Simulation$allFloatingMoleculeNames()`](#method-Simulation-allFloatingMoleculeNames)

- [`Simulation$molWeightFor()`](#method-Simulation-molWeightFor)

- [`Simulation$allApplicationsFor()`](#method-Simulation-allApplicationsFor)

- [`Simulation$print()`](#method-Simulation-print)

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

### Method `new()`

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

### Method `allEndogenousStationaryMoleculeNames()`

Returns the name of all endogenous stationary molecules defined in the
simulation. (e.g. with the flag IsStationary = TRUE) This is a typically
a molecule that is individual specific such as en Enzyme, Protein,
Transporter, FcRn etc.

#### Usage

    Simulation$allEndogenousStationaryMoleculeNames()

------------------------------------------------------------------------

### Method `allXenobioticFloatingMoleculeNames()`

Returns the name of all xenobiotic floating molecules defined in the
simulation. (e.g. with the flag IsStationary = FALSE) This is typically
a molecule that is being explicitly simulated such as Compound,
Inhibitor, DrugComplex.

#### Usage

    Simulation$allXenobioticFloatingMoleculeNames()

------------------------------------------------------------------------

### Method `allStationaryMoleculeNames()`

Returns the name of all stationary molecules defined in the simulation.
(e.g. with the flag IsStationary = TRUE)

#### Usage

    Simulation$allStationaryMoleculeNames()

------------------------------------------------------------------------

### Method `allFloatingMoleculeNames()`

Returns the name of all floating molecules defined in the simulation.
(e.g. with the flag IsStationary = FALSE)

#### Usage

    Simulation$allFloatingMoleculeNames()

------------------------------------------------------------------------

### Method `molWeightFor()`

Returns the mol weight value (in core unit) associated to the quantity
with given path or NA if not found

#### Usage

    Simulation$molWeightFor(quantityPath)

#### Arguments

- `quantityPath`:

  Path of quantity used to retrieve the molecular weight

------------------------------------------------------------------------

### Method `allApplicationsFor()`

Returns the applications ordered by start time associated to the
quantity with path `quantityPath` or an empty list if not found

#### Usage

    Simulation$allApplicationsFor(quantityPath)

#### Arguments

- `quantityPath`:

  Path of quantity used to retrieve the applications (e.g. applications
  resulting in this quantity being applied)

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    Simulation$print(...)

#### Arguments

- `...`:

  Rest arguments.
