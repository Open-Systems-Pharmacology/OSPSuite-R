# importAllSheets parameter is deprecated

    Code
      dataSets <- loadDataSetsFromExcel(xlsFilePath = xlsPath,
        importerConfigurationOrPath = configPath, importAllSheets = TRUE)
    Condition
      Warning:
      The `importAllSheets` argument of `loadDataSetsFromExcel()` is deprecated as of ospsuite 12.4.2.
      i Please use the `sheets` argument instead.
      i Use `sheets = NULL` to load all sheets. This parameter will be removed in version 14.

# dataSetsFromDataFrame throws an error for conflicting values in the column it uses

    Code
      dataSetsFromDataFrame(df)
    Condition
      Error in `.getSingleNonNaValue()`:
      ! Column 'OutputPath' has conflicting values within dataset 'A'.

