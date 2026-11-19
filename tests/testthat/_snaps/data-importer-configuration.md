# It can print the configuration

    Code
      print(importerConfiguration)
    Output
      <DataImporterConfiguration>
        * Time column: Time
        * Time unit: h
        * Time unit from column: FALSE
        * Measurement column: Measurement
        * Measurement unit: µmol/l
        * Measurement unit from column: FALSE
        * LLOQ column: NULL
        * Error column: NULL
        * Error type: NULL
        * Error type column: NULL
        * Error unit: NULL
        * Grouping columns: foo, tata
        * Sheets: <empty vector>
        * Naming pattern: {Source}.{Sheet}

# It can print the configuration with an error type column

    Code
      print(importerConfiguration)
    Output
      <DataImporterConfiguration>
        * Time column: Time
        * Time unit: h
        * Time unit from column: FALSE
        * Measurement column: Measurement
        * Measurement unit: µmol/l
        * Measurement unit from column: FALSE
        * LLOQ column: NULL
        * Error column: Error
        * Error type: ArithmeticStdDev
        * Error type column: Error type
        * Error unit: µmol/l
        * Grouping columns: <empty vector>
        * Sheets: <empty vector>
        * Naming pattern: {Source}.{Sheet}

# loadDataSetsFromExcel reads the error type of each data set from a column

    Code
      errorSummary(dataSets)
    Output
        name       yErrorType yErrorUnit yErrorValues
      1    A  GeometricStdDev                    2, 3
      2    B ArithmeticStdDev       mg/l         1, 2
      3    C ArithmeticStdDev       mg/l         1, 2

# loadDataSetsFromExcel applies errorType GeometricStdDev to data sets without a value in the error type column

    Code
      errorSummary(dataSets)
    Output
        name       yErrorType yErrorUnit yErrorValues
      1    A  GeometricStdDev                    2, 3
      2    B  GeometricStdDev                    1, 2
      3    C ArithmeticStdDev       mg/l         1, 2

# the error unit can be set in a configuration created for a file with a geometric error

    Code
      errorSummary(dataSets)
    Output
        name       yErrorType yErrorUnit yErrorValues
      1    A  GeometricStdDev                    2, 3
      2    B  GeometricStdDev                    1, 2
      3    C ArithmeticStdDev       mg/l         1, 2

# the error type column is kept when saving and loading a configuration

    Code
      errorSummary(dataSets)
    Output
        name       yErrorType yErrorUnit yErrorValues
      1    A  GeometricStdDev                    2, 3
      2    B  GeometricStdDev                    1, 2
      3    C ArithmeticStdDev       mg/l         1, 2

