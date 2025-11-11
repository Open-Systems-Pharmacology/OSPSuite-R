# Calculating PK parameters of simulation outputs

## Calculating PK parameters

The [ospsuite](https://github.com/open-systems-pharmacology/ospsuite-r)
package gives you access to the standard PK Parameters calculated by
PK-Sim such as AUC, Cmax etc. For the complete list of PK Parameters
supported out of the box, please refer to the [online
documentation](https://docs.open-systems-pharmacology.org/working-with-pk-sim/pk-sim-documentation/pk-sim-simulations#pk-parameters).

PK parameters can be calculated for all outputs of a simulation. First,
simulation results must be calculated, and the `SimulationResults`
object is then passed to the method `calculatePKAnalyses`. For the list
of calculated PK parameters and their description, please refer to [OSPS
documentation](https://docs.open-systems-pharmacology.org/working-with-pk-sim/pk-sim-documentation/pk-sim-simulations#pk-analysis-view).

``` r
library(ospsuite)

# Load the simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

# Run the simulation
simulationResults <- runSimulations(simulations = sim)[[1]]

# Calculate PK-analyses
pkAnalysis <- calculatePKAnalyses(results = simulationResults)

# Get the path of the simulated output
outputPath <- simulationResults$allQuantityPaths[[1]]
print(outputPath)
#> [1] "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

# Get all calculated PK parameters for the output path
allPkParams <- pkAnalysis$allPKParametersFor(outputPath)
print(allPkParams)
#> [[1]]
#> <QuantityPKParameter>
#>   • Name: C_max
#>   • QuantityPath: Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral
#>   Venous Blood)
#>   • Dimension: Concentration (molar)
#>   • Unit: µmol/l
#> 
#> [[2]]
#> <QuantityPKParameter>
#>   • Name: C_max_norm
#>   • QuantityPath: Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral
#>   Venous Blood)
#>   • Dimension: Concentration (mass)
#>   • Unit: kg/l
#> 
#> [[3]]
#> <QuantityPKParameter>
#>   • Name: t_max
#>   • QuantityPath: Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral
#>   Venous Blood)
#>   • Dimension: Time
#>   • Unit: min
#> 
#> [[4]]
#> <QuantityPKParameter>
#>   • Name: C_tEnd
#>   • QuantityPath: Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral
#>   Venous Blood)
#>   • Dimension: Concentration (molar)
#>   • Unit: µmol/l
#> 
#> [[5]]
#> <QuantityPKParameter>
#>   • Name: AUC_tEnd
#>   • QuantityPath: Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral
#>   Venous Blood)
#>   • Dimension: AUC (molar)
#>   • Unit: µmol*min/l
#> 
#> [[6]]
#> <QuantityPKParameter>
#>   • Name: AUC_tEnd_norm
#>   • QuantityPath: Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral
#>   Venous Blood)
#>   • Dimension: AUC (mass)
#>   • Unit: kg*min/l
#> 
#> [[7]]
#> <QuantityPKParameter>
#>   • Name: AUC_inf
#>   • QuantityPath: Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral
#>   Venous Blood)
#>   • Dimension: AUC (molar)
#>   • Unit: µmol*min/l
#> 
#> [[8]]
#> <QuantityPKParameter>
#>   • Name: AUC_inf_norm
#>   • QuantityPath: Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral
#>   Venous Blood)
#>   • Dimension: AUC (mass)
#>   • Unit: kg*min/l
#> 
#> [[9]]
#> <QuantityPKParameter>
#>   • Name: MRT
#>   • QuantityPath: Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral
#>   Venous Blood)
#>   • Dimension: Time
#>   • Unit: min
#> 
#> [[10]]
#> <QuantityPKParameter>
#>   • Name: Thalf
#>   • QuantityPath: Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral
#>   Venous Blood)
#>   • Dimension: Time
#>   • Unit: min
#> 
#> [[11]]
#> <QuantityPKParameter>
#>   • Name: FractionAucLastToInf
#>   • QuantityPath: Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral
#>   Venous Blood)
#>   • Dimension: Fraction
#> 
#> [[12]]
#> <QuantityPKParameter>
#>   • Name: CL
#>   • QuantityPath: Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral
#>   Venous Blood)
#>   • Dimension: Flow per weight
#>   • Unit: l/min/kg
#> 
#> [[13]]
#> <QuantityPKParameter>
#>   • Name: Vss
#>   • QuantityPath: Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral
#>   Venous Blood)
#>   • Dimension: Volume per body weight
#>   • Unit: l/kg
#> 
#> [[14]]
#> <QuantityPKParameter>
#>   • Name: Vd
#>   • QuantityPath: Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral
#>   Venous Blood)
#>   • Dimension: Volume per body weight
#>   • Unit: l/kg

# Get C_max parameter
c_max <- pkAnalysis$pKParameterFor(quantityPath = outputPath, pkParameter = "C_max")
c_max
#> <QuantityPKParameter>
#>   • Name: C_max
#>   • QuantityPath: Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral
#>   Venous Blood)
#>   • Dimension: Concentration (molar)
#>   • Unit: µmol/l
```

The function `calculatePKAnalyses` returns an object of the class
`SimulationPKAnalyses`, which allows to retrieve either a certain PK
parameter for an output path, or all calculated PK parameters for an
output path.

The methods `$allPKParametersFor()` and `$pKParameterFor()` return a
object (or a list of objects) of the class `QuantityPKParameter`, with
the fields `$name` which is the name of the pk-parameter (e.g. “C_max”),
`$quantityPath` the path of the output the parameter has been calculated
for, and `$values` the value (or list of values for a population
simulation).

``` r
# Get C_max parameter
c_max <- pkAnalysis$pKParameterFor(quantityPath = outputPath, pkParameter = "C_max")

print(c_max)
#> <QuantityPKParameter>
#>   • Name: C_max
#>   • QuantityPath: Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral
#>   Venous Blood)
#>   • Dimension: Concentration (molar)
#>   • Unit: µmol/l

c_max$name
#> [1] "C_max"
c_max$quantityPath
#> [1] "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
c_max$values
#> [1] 50.25272
```

In case of a population simulation, `$values` return a list of values
calculated for each individual.

## Extracting a dataframe

Sometimes it is desirable to have the calculated parameters in a
dataframe for working with them further in other workflows (e.g. data
wrangling), and the package provides a convenient helper to extract it:

``` r
pkAnalysesToDataFrame(pkAnalysis)
#> # A tibble: 14 × 5
#>    IndividualId QuantityPath                            Parameter    Value Unit 
#>           <int> <chr>                                   <chr>        <dbl> <chr>
#>  1            0 Organism|PeripheralVenousBlood|Aciclov… C_max     5.03e+ 1 µmol…
#>  2            0 Organism|PeripheralVenousBlood|Aciclov… C_max_no… 3.07e+ 6 mg/l 
#>  3            0 Organism|PeripheralVenousBlood|Aciclov… t_max     1.83e- 1 h    
#>  4            0 Organism|PeripheralVenousBlood|Aciclov… C_tEnd    3.25e- 2 µmol…
#>  5            0 Organism|PeripheralVenousBlood|Aciclov… AUC_tEnd  4.06e+ 3 µmol…
#>  6            0 Organism|PeripheralVenousBlood|Aciclov… AUC_tEnd… 2.48e+11 µg*m…
#>  7            0 Organism|PeripheralVenousBlood|Aciclov… AUC_inf   4.07e+ 3 µmol…
#>  8            0 Organism|PeripheralVenousBlood|Aciclov… AUC_inf_… 2.48e+11 µg*m…
#>  9            0 Organism|PeripheralVenousBlood|Aciclov… MRT       3.22e+ 0 h    
#> 10            0 Organism|PeripheralVenousBlood|Aciclov… Thalf     3.02e+ 0 h    
#> 11            0 Organism|PeripheralVenousBlood|Aciclov… Fraction… 2.09e- 3 NA   
#> 12            0 Organism|PeripheralVenousBlood|Aciclov… CL        4.02e+ 0 ml/m…
#> 13            0 Organism|PeripheralVenousBlood|Aciclov… Vss       7.78e+ 2 ml/kg
#> 14            0 Organism|PeripheralVenousBlood|Aciclov… Vd        1.05e+ 3 ml/kg
```

## Import and export of PK-analyses

Population analysis calculated in R can be exported to a \*.csv file and
loaded in PK-Sim, and vice versa.

``` r
# Load and run the simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)
simulationResults <- runSimulations(simulations = sim)[[1]]

# Calculate PK-analysis
pkAnalysis <- calculatePKAnalyses(results = simulationResults)

# Export to csv
csvPKAnalysisPath <- system.file("extdata", "PKAnalyses.csv", package = "ospsuite")
exportPKAnalysesToCSV(pkAnalyses = pkAnalysis, filePath = csvPKAnalysisPath)

# Load from csv
pkAnalysisLoaded <- importPKAnalysesFromCSV(filePath = csvPKAnalysisPath, simulation = sim)
```

## User-Defined PK Parameters

The [ospsuite](https://github.com/open-systems-pharmacology/ospsuite-r)
package also supports user-defined PK Parameters, e.g. PK Parameter that
can be tailored to specific project needs. This feature is useful when
calculating PK Parameters for specific time intervals, or to apply PK
Parameters for output not using the dimension `Concentration`.

A User-Defined PK Parameter is always based on an existing PK-Parameter
and simply extends the way the output is calculated. It is not possible
at the moment to define your own `formula`.

The utility
[`addUserDefinedPKParameter`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/addUserDefinedPKParameter.html)
creates and returns an instance of the
[`UserDefinedPKParameter`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/UserDefinedPKParameter.html)
which can then be parameterized for specific requirements.

### Examples

#### Calculate AUC between for a specific time interval.

``` r
# Adds a user defined parameter named `MyAuc` that will calculate the value of AUC between t=50 min and t=80min only.

# Create a new parameter based on the standard AUC parameter
myAUC <- addUserDefinedPKParameter(
  name = "MyAUC",
  standardPKParameter = StandardPKParameter$AUC_tEnd
)

# Specifies start time and end time in minute
myAUC$startTime <- 50
myAUC$endTime <- 80
```

#### Calculate CMax between the 4th and 5th application of a multiple applications simulation.

``` r
# Adds a user defined parameter named `MyCMax` that will calculate the value of Cmax between the 4th and 5th application

# Create a new parameter based on the standard C_max parameter
myCMax <- addUserDefinedPKParameter(
  name = "MyCMax",
  standardPKParameter = StandardPKParameter$C_max
)

# Specifies start application (4th) and end application (5th)
myCMax$startApplicationIndex <- 4
myCMax$endApplicationIndex <- 5
```

#### Calculate CMax between the 4th and 5th application of a multiple applications simulation and apply a time offset

``` r
# Adds a user defined parameter named `MyCMax` that will calculate the value of Cmax between the 4th application start time + 10 min and
# the 5th application start time + 20min

# Create a new parameter based on the standard C_max parameter
myCMax <- addUserDefinedPKParameter(
  name = "MyCMax",
  standardPKParameter = StandardPKParameter$C_max
)

# Specifies start application (4th) and end application (5th)
myCMax$startApplicationIndex <- 4
myCMax$endApplicationIndex <- 5

# Specifies start time offset. The PK calculations will effectively start at StartTime of 4th Application + 10 min
myCMax$startTimeOffset <- 10

# Specifies end time offset. The PK calculations will effectively ends at StartTime of 5th Application + 20 min
myCMax$endTimeOffset <- 20
```

#### Update Dimension of a standard parameter for correct visualization

PK parameters currently always assume that the underlying quantity is a
concentration and do not check for units. If the quantity of interest
has another dimension, creating a user-defined PK parameter `AND`
setting its display unit to any valid unit of the target dimension can
do the trick

``` r
# Let's assume there is an observer called Q_observer in mg/m2 using the dimension Dose per body surface area.
# Simply using C_max would result in the parameter being shown in umol\l.
# To see the correct unit and dimension, the following can be done:

QMax <- addUserDefinedPKParameter(
  name = "Q_max",
  standardPKParameter = StandardPKParameter$C_max,
  displayName = "Q max",
  displayUnit = "mg/m²"
)
```

What will happen here:

- The dimension of the user defined PK Parameter will be estimated from
  the display unit, e.g in this case `mg/m²`
- The calculated value of the user defined PK Parameter will be
  interpreted as value in the base unit of this dimension, e.g. here
  “kg/dm²”
- If the display unit differs from the base unit of the dimension,
  calculated value will be converted into the target display unit

E.g. in the example above:

- First, the C_max value of the quantity will be calculated (because the
  user defined parameter is derived from C_max)
- From the display unit `mg/m²`, the dimension will be estimated (“Dose
  per body surface area”)
- The calculated C_max value will be interpreted as a value in `kg/dm²`
- Finally, the calculated value will be converted to `mg/m²` and set in
  the user defined PK Parameter.

To see all other options for user defined PK Parameter, refer to the
documentation of `UserDefinedPKParameter` (?UserDefinedPKParameter in
RStudio)
