# dataType can be changed after dataset is added

    Code
      myDC$setDataTypes(names = c("Observed", "Simulated"), dataTypes = c("observed",
        "simulated"))

# It can print data combined

    Code
      print(dataCombined)
    Output
      <DataCombined>
      
      -- Datasets and groupings: -----------------------------------------------------
      
      # A tibble: 2 x 3
        name         group      dataType
        <chr>        <chr>      <chr>   
      1 withError    firstGroup observed
      2 withoutError <NA>       observed

