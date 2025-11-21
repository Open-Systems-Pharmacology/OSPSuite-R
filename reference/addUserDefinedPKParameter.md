# Adds and returns a User-Defined PK-Parameter to the managed list of PK-Parameters

Adds and returns a User-Defined PK-Parameter to the managed list of
PK-Parameters

## Usage

``` r
addUserDefinedPKParameter(
  name,
  standardPKParameter,
  displayName = NULL,
  displayUnit = NULL
)
```

## Arguments

- name:

  Name of the user defined PK-Parameter

- standardPKParameter:

  Defined the standard PK-Parameter to use to perform the calculations

- displayName:

  Display Name to use when exporting the values (optional, default value
  is name)

- displayUnit:

  Unit in which the value will be exported

## Value

The newly created `UserDefinedPKParameter`that was added to the list of
PK-Parameters

## Examples

``` r
# Adds a user defined parameter named MyAuc that will calculate the value of AUC
# between t=50 min and t=80min
myAUC <- addUserDefinedPKParameter(
  name = "MyAUC",
  standardPKParameter = StandardPKParameter$AUC_tEnd
)
myAUC$startTime <- 50
myAUC$endTime <- 80

# Adds a user defined parameter named MyCMax that will calculate the value of Cmax
# between the 4th and 5th application
myCMax <- addUserDefinedPKParameter(
  name = "MyCMax",
  standardPKParameter = StandardPKParameter$C_max
)
myCMax$startApplicationIndex <- 4
myCMax$endApplicationIndex <- 5
```
