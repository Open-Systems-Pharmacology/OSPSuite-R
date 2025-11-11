# Get the value of a global ospsuite-R setting.

Get the value of a global ospsuite-R setting.

## Usage

``` r
getOSPSuiteSetting(settingName)
```

## Arguments

- settingName:

  String name of the setting

## Value

Value of the setting stored in ospsuiteEnv. If the setting does not
exist, an error is thrown.

## Examples

``` r
getOSPSuiteSetting("suiteVersion")
#> [1] "12"
getOSPSuiteSetting("sensitivityAnalysisConfig")$totalSensitivityThreshold
#> [1] 0.9
```
