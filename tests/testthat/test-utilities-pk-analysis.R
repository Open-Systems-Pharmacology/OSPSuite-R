# calculatePKAnalyses

sim <- loadSimulation(aciclovirSimulationPath, loadFromCache = TRUE)
outputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
results <- runSimulations(sim)[[1]]
pkAnalyses <- calculatePKAnalyses(results)

test_that("It should be able to calculate the PK-Analyses each output of a simulation", {
  pkAnalysesForOutput <- pkAnalyses$allPKParametersFor(
    outputPath
  )
  expect_gt(length(pkAnalysesForOutput), 0)
})

test_that("It should be able to retrieve a standard pk parameter", {
  pkParameter <- pkAnalyses$pKParameterFor(
    outputPath,
    "AUC_tEnd"
  )
  expect_false(is.null(pkParameter))
})

test_that("It should return null when retrieving a pk parameter that does not exist", {
  pkParameter <- pkAnalyses$pKParameterFor(
    outputPath,
    "NOPE"
  )
  expect_null(pkParameter)
})

test_that("it can return pk analysis for sparse population and only return ids defined in the population file", {
  population <- loadPopulation(getTestDataFilePath("pop_5_spared_id.csv"))
  results <- runSimulations(sim, population = population)[[1]]
  pkAnalyses <- calculatePKAnalyses(results = results)
  df <- pkAnalysesToDataFrame(pkAnalyses)
  d <- dplyr::distinct(df, IndividualId)
  expect_equal(nrow(d), 5)
})


test_that("It returns an empty list of parameters for an output that is not part of the calculated results", {
  pkAnalysesForOutput <- pkAnalyses$allPKParametersFor(
    "Another output that does not exist"
  )
  expect_equal(length(pkAnalysesForOutput), 0)
})

# exportPKAnalysesToCSV

test_that("It can export valid pk-analyses results to CSV", {
  executeWithTestFile(function(csvFile) {
    exportPKAnalysesToCSV(pkAnalyses, csvFile)
    expect_true(file.exists(csvFile))
  })
})

# pkAnalysesToDataFrame

# Reference: the conversion used before #794 (export to a file, read it back)
pkAnalysesFromFile <- function(pkAnalyses) {
  filePath <- tempfile()
  on.exit(file.remove(filePath))
  exportPKAnalysesToCSV(pkAnalyses, filePath)
  readr::read_csv(
    filePath,
    locale = readr::locale(encoding = "UTF-8"),
    comment = "#",
    col_types = readr::cols(
      IndividualId = readr::col_integer(),
      QuantityPath = readr::col_character(),
      Parameter = readr::col_character(),
      Value = readr::col_double(),
      Unit = readr::col_character()
    ),
    na = c("NaN", "", NA, "NA", "Infinity", "-Infinity")
  )
}

expectPKAnalysesColumns <- function(df) {
  expect_s3_class(df, "tbl_df")
  expect_named(
    df,
    c("IndividualId", "QuantityPath", "Parameter", "Value", "Unit")
  )
  expect_type(df$IndividualId, "integer")
  expect_type(df$QuantityPath, "character")
  expect_type(df$Parameter, "character")
  expect_type(df$Value, "double")
  expect_type(df$Unit, "character")
}

# Not added to the cache: the outputs added here must not leak into other tests
simMultipleOutputs <- loadSimulation(
  aciclovirSimulationPath,
  addToCache = FALSE
)
addOutputs("Organism|VenousBlood|*|Aciclovir", simMultipleOutputs)
pkAnalysesMultipleOutputs <- calculatePKAnalyses(
  runSimulations(simMultipleOutputs)[[1]]
)
pkAnalysesPopulation <- calculatePKAnalyses(
  runSimulations(
    simMultipleOutputs,
    population = loadPopulation(getTestDataFilePath("pop_5_spared_id.csv"))
  )[[1]]
)
allPathsMultiple <- pkAnalysesMultipleOutputs$allQuantityPaths

# Calls pkAnalysesToDataFrame() with filtering forced to .NET or to R
pkAnalysesToDataFrameFilteredIn <- function(filterInNet, ...) {
  local_mocked_bindings(.isFilterInNetFaster = function(...) filterInNet)
  pkAnalysesToDataFrame(...)
}

test_that("It can convert valid pk-analysis results to data frame", {
  df <- pkAnalysesToDataFrame(pkAnalyses = pkAnalyses)
  expect_length(colnames(df), 5)
})

test_that("pkAnalysesToDataFrame returns the same data as the file export for several outputs", {
  df <- pkAnalysesToDataFrame(pkAnalysesMultipleOutputs)
  expectPKAnalysesColumns(df)
  expect_gt(length(unique(df$QuantityPath)), 1)
  expect_equal(
    df,
    pkAnalysesFromFile(pkAnalysesMultipleOutputs),
    ignore_attr = TRUE,
    tolerance = 0
  )
})

test_that("pkAnalysesToDataFrame returns the same data as the file export for a population", {
  df <- pkAnalysesToDataFrame(pkAnalysesPopulation)
  expectPKAnalysesColumns(df)
  expect_equal(length(unique(df$IndividualId)), 5)
  expect_gt(length(unique(df$QuantityPath)), 1)
  expect_equal(
    df,
    pkAnalysesFromFile(pkAnalysesPopulation),
    ignore_attr = TRUE,
    tolerance = 0
  )
})

test_that("Values are parsed to the same doubles as the file export", {
  # as.numeric() and data.table::fread() give the next double for these values,
  # readr::read_csv() (used by the file export) gives the nearest double
  csv <- paste0(
    "\"IndividualId\",\"QuantityPath\",\"Parameter\",\"Value\",\"Unit\"\r\n",
    "0,\"A|B\",\"C_max\",3.8367824E-07,mg\r\n",
    "1,\"A|B\",\"C_max\",0.640403,mg\r\n"
  )
  df <- .pkAnalysesCSVToDataFrame(csv)
  expect_identical(
    df$Value,
    readr::parse_double(c("3.8367824E-07", "0.640403"))
  )
  expect_identical(
    sprintf("%a", df$Value),
    c("0x1.9bf8abeac6d3fp-22", "0x1.47e2e6ea85447p-1")
  )
})

test_that("pkAnalysesToDataFrame keeps the micro sign of units", {
  df <- pkAnalysesToDataFrame(pkAnalysesMultipleOutputs)
  expect_true(enc2utf8("\u00b5mol/l") %in% df$Unit)
})

test_that("pkAnalysesToDataFrame does not create a temporary file", {
  # Watches R's tempfile() and the session temporary folder. Files that .NET
  # might write elsewhere are not covered; PKAnalysesToCSV writes none.
  counter <- new.env()
  counter$calls <- 0
  suppressMessages(trace(
    "tempfile",
    tracer = bquote(
      assign("calls", .(counter)$calls + 1, envir = .(counter))
    ),
    where = baseenv(),
    print = FALSE
  ))
  on.exit(
    suppressMessages(untrace("tempfile", where = baseenv())),
    add = TRUE
  )
  filesBefore <- list.files(tempdir(), all.files = TRUE, recursive = TRUE)

  pkAnalysesToDataFrame(pkAnalysesMultipleOutputs)

  expect_equal(counter$calls, 0)
  expect_equal(
    list.files(tempdir(), all.files = TRUE, recursive = TRUE),
    filesBefore
  )
})

test_that("The CSV text from .NET is parsed with special values as NA", {
  csv <- paste0(
    "\"IndividualId\",\"QuantityPath\",\"Parameter\",\"Value\",\"Unit\"\r\n",
    "0,\"A|B\",\"C_max\",1.5,\u00b5mol/l\r\n",
    "1,\"A|B\",\"C_max\",NaN,\u00b5mol/l\r\n",
    "2,\"A|B\",\"C_max\",Infinity,\u00b5mol/l\r\n",
    "3,\"A|B\",\"C_max\",-Infinity,\u00b5mol/l\r\n",
    "4,\"A, C|B\",\"C_max\",,\r\n",
    "5,\"A|B\",\"t_max\",1E-05,h\r\n"
  )
  df <- .pkAnalysesCSVToDataFrame(csv)
  expectPKAnalysesColumns(df)
  expect_equal(df$IndividualId, 0:5)
  expect_equal(df$Value, c(1.5, NA, NA, NA, NA, 1e-05))
  expect_equal(df$QuantityPath[[5]], "A, C|B")
  expect_equal(df$Unit, c(rep(enc2utf8("\u00b5mol/l"), 4), NA, "h"))
})

test_that("The CSV text from .NET with a header only gives an empty data frame", {
  df <- .pkAnalysesCSVToDataFrame(
    "\"IndividualId\",\"QuantityPath\",\"Parameter\",\"Value\",\"Unit\"\r\n"
  )
  expectPKAnalysesColumns(df)
  expect_equal(nrow(df), 0)
})

test_that("A value that is not a number becomes NA with a warning and the column stays numeric", {
  csv <- paste0(
    "\"IndividualId\",\"QuantityPath\",\"Parameter\",\"Value\",\"Unit\"\r\n",
    "0,\"A|B\",\"C_max\",\u221e,mg\r\n",
    "1,\"A|B\",\"C_max\",2,mg\r\n"
  )
  expect_warning(df <- .pkAnalysesCSVToDataFrame(csv), "parsing failure")
  expectPKAnalysesColumns(df)
  expect_equal(df$Value, c(NA, 2))
})

test_that("pkAnalysesToDataFrame ignores the readr default locale option", {
  expected <- pkAnalysesToDataFrame(pkAnalysesPopulation)
  withr::local_options(
    readr.default_locale = readr::locale(
      decimal_mark = ",",
      grouping_mark = "."
    )
  )
  expect_equal(
    pkAnalysesToDataFrame(pkAnalysesPopulation),
    expected,
    tolerance = 0
  )
})

# pkAnalysesToDataFrame filters

test_that("pkAnalysesToDataFrame filters by quantity paths", {
  full <- pkAnalysesToDataFrame(pkAnalysesMultipleOutputs)
  paths <- allPathsMultiple[c(3, 1)]
  for (filterInNet in c(TRUE, FALSE)) {
    df <- pkAnalysesToDataFrameFilteredIn(
      filterInNet,
      pkAnalysesMultipleOutputs,
      quantitiesOrPaths = paths
    )
    expectPKAnalysesColumns(df)
    expect_equal(df, full[full$QuantityPath %in% paths, ], tolerance = 0)
    expect_equal(unique(df$QuantityPath), allPathsMultiple[c(1, 3)])
  }
})

test_that("pkAnalysesToDataFrame filters by quantity objects, also mixed with paths", {
  full <- pkAnalysesToDataFrame(pkAnalysesMultipleOutputs)
  quantity <- getQuantity(allPathsMultiple[[2]], simMultipleOutputs)
  for (filterInNet in c(TRUE, FALSE)) {
    df <- pkAnalysesToDataFrameFilteredIn(
      filterInNet,
      pkAnalysesMultipleOutputs,
      quantitiesOrPaths = quantity
    )
    expectPKAnalysesColumns(df)
    expect_equal(
      df,
      full[full$QuantityPath == allPathsMultiple[[2]], ],
      tolerance = 0
    )

    df <- pkAnalysesToDataFrameFilteredIn(
      filterInNet,
      pkAnalysesMultipleOutputs,
      quantitiesOrPaths = list(quantity, allPathsMultiple[[4]])
    )
    expect_equal(unique(df$QuantityPath), allPathsMultiple[c(2, 4)])
  }
})

test_that("pkAnalysesToDataFrame filters by PK parameters", {
  full <- pkAnalysesToDataFrame(pkAnalysesPopulation)
  pkParameters <- c("t_max", "C_max", "C_max")
  for (filterInNet in c(TRUE, FALSE)) {
    df <- pkAnalysesToDataFrameFilteredIn(
      filterInNet,
      pkAnalysesPopulation,
      pkParameters = pkParameters
    )
    expectPKAnalysesColumns(df)
    expect_equal(df, full[full$Parameter %in% pkParameters, ], tolerance = 0)
  }
})

test_that("pkAnalysesToDataFrame combines both filters and ignores duplicates", {
  full <- pkAnalysesToDataFrame(pkAnalysesPopulation)
  paths <- allPathsMultiple[c(2, 4, 2)]
  pkParameters <- c("AUC_inf", "Thalf")
  for (filterInNet in c(TRUE, FALSE)) {
    df <- pkAnalysesToDataFrameFilteredIn(
      filterInNet,
      pkAnalysesPopulation,
      quantitiesOrPaths = paths,
      pkParameters = pkParameters
    )
    expect_equal(
      df,
      full[full$QuantityPath %in% paths & full$Parameter %in% pkParameters, ],
      tolerance = 0
    )
    expect_equal(nrow(df), 2 * 2 * 5)
  }
})

test_that("pkAnalysesToDataFrame returns all rows when the filters list everything", {
  df <- pkAnalysesToDataFrame(
    pkAnalysesMultipleOutputs,
    quantitiesOrPaths = allPathsMultiple,
    pkParameters = pkAnalysesMultipleOutputs$allPKParameterNames
  )
  expect_equal(df, pkAnalysesToDataFrame(pkAnalysesMultipleOutputs))
})

test_that("pkAnalysesToDataFrame returns an empty data frame when the filters match nothing", {
  for (filterInNet in c(TRUE, FALSE)) {
    df <- pkAnalysesToDataFrameFilteredIn(
      filterInNet,
      pkAnalysesMultipleOutputs,
      quantitiesOrPaths = character(0)
    )
    expectPKAnalysesColumns(df)
    expect_equal(nrow(df), 0)

    df <- pkAnalysesToDataFrameFilteredIn(
      filterInNet,
      pkAnalysesMultipleOutputs,
      pkParameters = character(0)
    )
    expectPKAnalysesColumns(df)
    expect_equal(nrow(df), 0)
  }
})

test_that("Both filter routes group the rows by output for PK-Analyses not grouped by output", {
  # Import a file sorted by PK parameter, so the outputs are interleaved
  executeWithTestFile(function(csvFile) {
    exportPKAnalysesToCSV(pkAnalysesMultipleOutputs, csvFile)
    lines <- readLines(csvFile, encoding = "UTF-8")
    dataLines <- lines[-1]
    parameterNames <- sub('^[^,]*,"[^"]*","([^"]*)".*$', "\\1", dataLines)
    writeLines(
      c(lines[[1]], dataLines[order(parameterNames)]),
      csvFile,
      useBytes = TRUE
    )
    pkAnalysesSorted <- importPKAnalysesFromCSV(csvFile, simMultipleOutputs)
    paths <- pkAnalysesSorted$allQuantityPaths[c(2, 1)]
    full <- pkAnalysesToDataFrame(pkAnalysesSorted)
    # The unfiltered rows switch between outputs more often than once per output
    outputChanges <- length(rle(full$QuantityPath)$lengths) - 1
    expect_gt(outputChanges, length(pkAnalysesSorted$allQuantityPaths) - 1)
    expected <- full[full$QuantityPath %in% paths, ]
    expected <- expected[
      order(match(expected$QuantityPath, pkAnalysesSorted$allQuantityPaths)),
    ]

    viaNet <- pkAnalysesToDataFrameFilteredIn(
      TRUE,
      pkAnalysesSorted,
      quantitiesOrPaths = paths
    )
    viaR <- pkAnalysesToDataFrameFilteredIn(
      FALSE,
      pkAnalysesSorted,
      quantitiesOrPaths = paths
    )
    expect_equal(viaNet, expected, tolerance = 0)
    expect_equal(viaR, expected, tolerance = 0)
  })
})

test_that("pkAnalysesToDataFrame returns an empty data frame for a path and a PK parameter without common values", {
  # Remove "t_max" of the second output so that both names exist but not together
  executeWithTestFile(function(csvFile) {
    exportPKAnalysesToCSV(pkAnalysesMultipleOutputs, csvFile)
    lines <- readLines(csvFile, encoding = "UTF-8")
    drop <- grepl(allPathsMultiple[[2]], lines, fixed = TRUE) &
      grepl("\"t_max\"", lines, fixed = TRUE)
    writeLines(lines[!drop], csvFile, useBytes = TRUE)
    pkAnalysesPartial <- importPKAnalysesFromCSV(csvFile, simMultipleOutputs)

    for (filterInNet in c(TRUE, FALSE)) {
      df <- pkAnalysesToDataFrameFilteredIn(
        filterInNet,
        pkAnalysesPartial,
        quantitiesOrPaths = allPathsMultiple[[2]],
        pkParameters = "t_max"
      )
      expectPKAnalysesColumns(df)
      expect_equal(nrow(df), 0)
    }
  })
})

test_that("pkAnalysesToDataFrame returns an empty data frame for PK-Analyses without outputs", {
  emptyPKAnalyses <- SimulationPKAnalyses$new(
    rSharp::newObjectFromName(
      "OSPSuite.Core.Domain.PopulationSimulationPKAnalyses"
    ),
    simMultipleOutputs
  )
  df <- pkAnalysesToDataFrame(emptyPKAnalyses)
  expectPKAnalysesColumns(df)
  expect_equal(nrow(df), 0)

  df <- pkAnalysesToDataFrame(emptyPKAnalyses, pkParameters = character(0))
  expectPKAnalysesColumns(df)
  expect_equal(nrow(df), 0)
})

test_that("pkAnalysesToDataFrame fails for unknown quantity paths or PK parameters", {
  expect_error(
    pkAnalysesToDataFrame(
      pkAnalysesMultipleOutputs,
      quantitiesOrPaths = c(allPathsMultiple[[1]], "Organism|Nope")
    ),
    "Organism|Nope",
    fixed = TRUE
  )
  expect_error(
    pkAnalysesToDataFrame(
      pkAnalysesMultipleOutputs,
      quantitiesOrPaths = "Organism|VenousBlood|*|Aciclovir"
    ),
    "Wildcards"
  )
  expect_error(
    pkAnalysesToDataFrame(
      pkAnalysesMultipleOutputs,
      pkParameters = c("C_max", "c_max")
    ),
    "c_max"
  )
})

test_that("pkAnalysesToDataFrame fails for arguments of the wrong type", {
  expect_error(
    pkAnalysesToDataFrame(pkAnalysesMultipleOutputs, quantitiesOrPaths = 1),
    "quantitiesOrPaths"
  )
  expect_error(
    pkAnalysesToDataFrame(pkAnalysesMultipleOutputs, pkParameters = 1),
    "pkParameters"
  )
  expect_error(
    pkAnalysesToDataFrame("not pk-analyses"),
    "SimulationPKAnalyses"
  )
})

test_that("pkAnalysesToDataFrame does not change the pk-analyses when filtering", {
  pathsBefore <- pkAnalysesPopulation$allQuantityPaths
  pkParametersBefore <- pkAnalysesPopulation$allPKParameterNames
  fullBefore <- pkAnalysesToDataFrame(pkAnalysesPopulation)

  for (filterInNet in c(TRUE, FALSE)) {
    pkAnalysesToDataFrameFilteredIn(
      filterInNet,
      pkAnalysesPopulation,
      quantitiesOrPaths = pathsBefore[[1]],
      pkParameters = "C_max"
    )
  }

  expect_equal(pkAnalysesPopulation$allQuantityPaths, pathsBefore)
  expect_equal(pkAnalysesPopulation$allPKParameterNames, pkParametersBefore)
  expect_equal(pkAnalysesToDataFrame(pkAnalysesPopulation), fullBefore)
})

test_that("Filtering in .NET is chosen only when it saves converting many rows", {
  # 500 individuals, 32 outputs x 14 PK parameters, one output selected
  expect_true(.isFilterInNetFaster(14, 448, 500))
  # Same population, all outputs but one selected
  expect_false(.isFilterInNetFaster(434, 448, 500))
  # One individual, 133 outputs x 14 PK parameters, C_max selected
  expect_false(.isFilterInNetFaster(133, 1862, 1))
})

test_that("pkAnalysesToTibble passes the filters on", {
  df <- pkAnalysesToTibble(
    pkAnalysesPopulation,
    quantitiesOrPaths = allPathsMultiple[[1]],
    pkParameters = "C_max"
  )
  expect_s3_class(df, "tbl_df")
  expect_equal(
    df,
    pkAnalysesToDataFrame(
      pkAnalysesPopulation,
      quantitiesOrPaths = allPathsMultiple[[1]],
      pkParameters = "C_max"
    )
  )
  expect_equal(nrow(df), 5)
})
