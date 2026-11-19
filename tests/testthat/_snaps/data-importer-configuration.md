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

