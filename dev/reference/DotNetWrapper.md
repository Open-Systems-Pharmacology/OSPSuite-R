# Wrapper class for `.NET` objects

Wrapper class for `.NET` objects

Wrapper class for `.NET` objects

## Super class

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\> `DotNetWrapper`

## Methods

### Public methods

- [`DotNetWrapper$new()`](#method-DotNetWrapper-new)

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
- [`rSharp::NetObject$print()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-print)
- [`rSharp::NetObject$set()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-set)

------------------------------------------------------------------------

### Method `new()`

Initialize a new instance of the class

#### Usage

    DotNetWrapper$new(netObject)

#### Arguments

- `netObject`:

  An
  [`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
  object.

#### Returns

A new `Molecule` object.

## Examples

``` r

sim <- loadSimulation(system.file("extdata", "simple.pkml", package = "ospsuite"))

# looking at a reference to `.NET` simulation object
sim$pointer
#> <pointer: 0x558d222897f0>

# create a new instance of `DotNetWrapper` class using this reference
DotNetWrapper$new(sim)
#> 
#> ── <DotNetWrapper> ──
#> 
#> Type: OSPSuite.R.Domain.Simulation
#> 
#> ── Available Methods 
#>   • `AcceptVisitor()`
#>   • `add_Changed()`
#>   • `add_PropertyChanged()`
#>   • `AddAnalysis()`
#>   • `All()`
#>   • `Equals()`
#>   • `get_Analyses()`
#>   • `get_BodyWeight()`
#>   • `get_BuildConfiguration()`
#>   • `get_Charts()`
#>   • `get_ComesFromPKSim()`
#>   • `get_CompoundNames()`
#>   • `get_Configuration()`
#>   • `get_CoreSimulation()`
#>   • `get_Creation()`
#>   • `get_Description()`
#>   • `get_EndTime()`
#>   • `get_EntitySources()`
#>   • `get_HasChanged()`
#>   • `get_HasUpToDateResults()`
#>   • `get_Icon()`
#>   • `get_Id()`
#>   • `get_IsLoaded()`
#>   • `get_Model()`
#>   • `get_Name()`
#>   • `get_OutputMappings()`
#>   • `get_OutputSelections()`
#>   • `get_Reactions()`
#>   • `get_ResultsDataRepository()`
#>   • `get_Settings()`
#>   • `GetHashCode()`
#>   • `GetType()`
#>   • `MolWeightFor()`
#>   • `MolWeightFor()`
#>   • `remove_Changed()`
#>   • `remove_PropertyChanged()`
#>   • `RemoveAnalysis()`
#>   • `RemoveOutputMappings()`
#>   • `RemoveUsedObservedData()`
#>   • `set_Configuration()`
#>   • `set_Creation()`
#>   • `set_Description()`
#>   • `set_EntitySources()`
#>   • `set_HasChanged()`
#>   • `set_Icon()`
#>   • `set_Id()`
#>   • `set_IsLoaded()`
#>   • `set_Model()`
#>   • `set_Name()`
#>   • `set_OutputMappings()`
#>   • `set_ResultsDataRepository()`
#>   • `ToString()`
#>   • `TotalDrugMassFor()`
#>   • `UpdatePropertiesFrom()`
#>   • `UsesObservedData()`
#> 
#> ── Available Properties 
#>   • Analyses
#>   • BodyWeight
#>   • BuildConfiguration
#>   • Charts
#>   • ComesFromPKSim
#>   • CompoundNames
#>   • Configuration
#>   • CoreSimulation
#>   • Creation
#>   • Description
#>   • EndTime
#>   • EntitySources
#>   • HasChanged
#>   • HasUpToDateResults
#>   • Icon
#>   • Id
#>   • IsLoaded
#>   • Model
#>   • Name
#>   • OutputMappings
#>   • OutputSelections
#>   • Reactions
#>   • ResultsDataRepository
#>   • Settings
```
