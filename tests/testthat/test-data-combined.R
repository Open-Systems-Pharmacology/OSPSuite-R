# empty initialization ----------------------------

test_that("dataCombined - initialization", {
  skip_if_not_installed("R6")

  # initialize empty object
  myCombDat <- DataCombined$new()

  # the active bindings should all be NULL at this stage
  expect_null(myCombDat$groupMap)
  expect_null(myCombDat$names)
  expect_null(myCombDat$toDataFrame())
  expect_output(print(myCombDat), "DataCombined:")

  # can't use active bindings like this
  expect_error(myCombDat$groupMap("x"))
  expect_error(myCombDat$names("x"))
  expect_error(myCombDat$dataTransformations("x"))

  # can't enter a list of `SimulationResults` objects
  expect_error(myCombDat$addSimulationResults(list("x", "y")))

  # can enter a list, but only of `DataSet` objects
  expect_error(myCombDat$addDataSets(list("x", "y")))
})

# either `DataSet` and `SimulationResults` provided -------------

test_that("dataCombined - either dataSet or SimulationResults provided", {
  skip_if_not_installed("R6")

  # load the simulation
  sim <- loadTestSimulation("MinimalModel")
  simResults <- importResultsFromCSV(
    simulation = sim,
    filePaths = getTestDataFilePath("Stevens_2012_placebo_indiv_results.csv")
  )

  # import observed data (will return a list of DataSet objects)
  dataSet <- loadDataSetsFromExcel(
    xlsFilePath = getTestDataFilePath("CompiledDataSetStevens2012.xlsx"),
    importerConfiguration = DataImporterConfiguration$new(getTestDataFilePath("ImporterConfiguration.xml"))
  )

  # only DataSet input

  myCombDat <- DataCombined$new()
  myCombDat$addDataSets(dataSet[[1]])

  expect_true(R6::is.R6(myCombDat))
  expect_false(R6::is.R6Class(myCombDat))

  # checking dataframe methods
  df <- myCombDat$toDataFrame()
  expect_s3_class(df, "data.frame")
  expect_equal(dim(df), c(12L, 20L))

  expect_equal(
    names(df),
    c(
      "group", "dataType", "name", "Group Id", "xValues", "xDimension",
      "xUnit", "yValues", "yErrorValues", "yDimension", "yUnit", "yErrorType",
      "yErrorUnit", "molWeight", "lloq", "Source", "Sheet", "Organ",
      "Compartment", "Molecule"
    )
  )
  expect_equal(df$name, df$group)
  expect_equal(unique(df$name), names(dataSet)[[1]])

  # only SimulationResults input

  myCombDat2 <- DataCombined$new()
  myCombDat2$addSimulationResults(simResults)

  expect_true(R6::is.R6(myCombDat2))
  expect_false(R6::is.R6Class(myCombDat2))

  # checking dataframe methods
  df2 <- myCombDat2$toDataFrame()
  expect_s3_class(df2, "data.frame")
  expect_equal(dim(df2), c(1255L, 9L))

  expect_equal(
    names(df2),
    c(
      "group", "dataType", "name",  "IndividualId", "xValues",  "xUnit", "yValues",
      "yUnit", "yDimension"
    )
  )
  expect_equal(unique(df2$name), simResults$allQuantityPaths)

  expect_equal(
    as.character(na.omit(unique(df2$name))),
    c(
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
      "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
      "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
      "Organism|Lumen|Stomach|Metformin|Gastric retention"
    )
  )

  expect_equal(df2$name, df2$group)
})

# both `DataSet` and `SimulationResults` provided -------------

test_that("dataCombined - both DataSet and SimulationResults provided", {
  skip_if_not_installed("R6")

  expect_true(R6::is.R6Class(DataCombined))

  # load the simulation
  sim <- loadTestSimulation("MinimalModel")
  simResults <- importResultsFromCSV(
    simulation = sim,
    filePaths = getTestDataFilePath("Stevens_2012_placebo_indiv_results.csv")
  )

  # import observed data (will return a list of DataSet objects)
  dataSet <- loadDataSetsFromExcel(
    xlsFilePath = getTestDataFilePath("CompiledDataSetStevens2012.xlsx"),
    importerConfiguration = DataImporterConfiguration$new(getTestDataFilePath("ImporterConfiguration.xml"))
  )

  # with list input ----------------------------

  # create object with datasets combined
  myCombDat <- DataCombined$new()
  myCombDat$addSimulationResults(
    simResults,
    quantitiesOrPaths = c(
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
      "Organism|Lumen|Stomach|Metformin|Gastric retention"
    ),
    names = list("x", "y", "z")
  )
  myCombDat$addDataSets(dataSet, names = list("a", NULL, "b", NULL, "c", NULL))

  expect_true(R6::is.R6(myCombDat))
  expect_false(R6::is.R6Class(myCombDat))

  # checking dataframe methods
  df <- myCombDat$toDataFrame()
  expect_s3_class(df, "data.frame")
  expect_equal(dim(df), c(830L, 21L))

  # check exact values
  expect_equal(
    names(df),
    c(
      "group", "dataType", "name", "IndividualId", "Group Id", "xValues",
      "xUnit", "xDimension", "yValues", "yUnit", "yDimension", "yErrorValues",
      "yErrorType", "yErrorUnit", "molWeight", "lloq", "Source", "Sheet",
      "Organ", "Compartment", "Molecule"
    )
  )

  expect_equal(df$name, df$group)

  # unchanged names should be same as in the original object
  expect_equal(
    unique(dplyr::filter(df, dataType == "observed")$name)[c(2, 4, 6)],
    names(dataSet)[c(2, 4, 6)]
  )

  # name changes have taken place for both types of data
  expect_equal(
    as.character(unique(df$name)),
    c(
      "x", "y", "z",
      "a",
      "Stevens_2012_placebo.Sita_total",
      "b",
      "Stevens_2012_placebo.Sita_proximal",
      "c",
      "Stevens_2012_placebo.Sita_dist"
    )
  )
  expect_equal(as.character(unique(df$name)), myCombDat$names)

  # read-only
  expect_error(myCombDat$dataSets(dataSet))
  expect_error(myCombDat$simulationResults(simResults))

  # not specified, so NULL
  expect_null(myCombDat$groupMap)

  # with DataSet input ----------------------------

  # create object with datasets combined
  myCombDat2 <- DataCombined$new()
  myCombDat2$addSimulationResults(simResults)
  myCombDat2$addDataSets(dataSet[[1]])

  expect_true(R6::is.R6(myCombDat2))
  expect_false(R6::is.R6Class(myCombDat2))

  # checking dataframe methods
  df2 <- myCombDat2$toDataFrame()
  expect_s3_class(df2, "data.frame")
  expect_equal(dim(df2), c(1267L, 21L))

  # not specified, so NULL
  expect_null(myCombDat$groupMap)

  expect_equal(
    as.character(unique(df2$name)),
    c(
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
      "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
      "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
      "Organism|Lumen|Stomach|Metformin|Gastric retention",
      "Stevens_2012_placebo.Placebo_total"
    )
  )

  # with nonsense list inputs ----------------------------

  myCombDat3 <- DataCombined$new()
  expect_error(myCombDat3$addDataSets(list("x" = 1, "y" = 2)))

  expect_error(myCombDat3$addSimulationResults(list(simResults, simResults)))

  # order should not matter ----------------------------

  # you can use one add method, extract a dataframe, and then add another add method
  # and then extract again; the dataframe method should then combine
  myCombDat4 <- DataCombined$new()
  myCombDat4$addSimulationResults(simResults)
  df3 <- myCombDat4$toDataFrame()
  myCombDat4$addDataSets(dataSet[[1]])
  df4 <- myCombDat4$toDataFrame()
  expect_equal(dim(df3), c(1255L, 9L))
  expect_equal(dim(df4), c(1267L, 21L))

  # method chaining works ----------------------------

  myCombDat5 <- DataCombined$new()
  df5 <- myCombDat5$addSimulationResults(simResults)$addDataSets(dataSet[[1]])$toDataFrame()
  expect_equal(dim(df4), dim(df5))

  myCombDat6 <- DataCombined$new()
  myCombDat6$addSimulationResults(simResults)$addDataSets(dataSet[[1]])$addDataSets(dataSet[[2]])
  expect_equal(
    myCombDat6$names,
    c(
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
      "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
      "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
      "Organism|Lumen|Stomach|Metformin|Gastric retention",
      "Stevens_2012_placebo.Placebo_total",
      "Stevens_2012_placebo.Sita_total"
    )
  )

  # not specified, so NULL
  expect_null(myCombDat$groupMap)

  # printing objects works
  expect_output(print(myCombDat))
  expect_output(print(myCombDat2))
  expect_output(print(myCombDat3))
  expect_output(print(myCombDat4))
  expect_output(print(myCombDat5))
})


# data transformations ---------------------------------

test_that("DataCombined with data transformations", {
  skip_if_not_installed("R6")

  # load the simulation
  sim <- loadTestSimulation("MinimalModel")
  simResults <- importResultsFromCSV(
    simulation = sim,
    filePaths = getTestDataFilePath("Stevens_2012_placebo_indiv_results.csv")
  )

  # import observed data (will return a list of DataSet objects)
  dataSet <- loadDataSetsFromExcel(
    xlsFilePath = getTestDataFilePath("CompiledDataSetStevens2012.xlsx"),
    importerConfiguration = DataImporterConfiguration$new(getTestDataFilePath("ImporterConfiguration.xml"))
  )

  # create object with datasets combined
  myCombDat <- DataCombined$new()

  # this should fail because of incorrect argument type
  expect_error(myCombDat$addSimulationResults(simResults, groups = 2))
  expect_error(myCombDat$addDataSets(dataSet, groups = c(2, 4)))

  myCombDat$addSimulationResults(simResults)
  myCombDat$addDataSets(dataSet)

  names_ls <- list(
    "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
    "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
    "Stevens_2012_placebo.Sita_proximal",
    "Stevens_2012_placebo.Placebo_distal"
  )

  # original dataframe
  dfOriginal <- myCombDat$toDataFrame()
  dfOriginal <- dplyr::filter(dfOriginal, name %in% names_ls)

  # expect error since the lengths of argument are not the same
  expect_error(
    myCombDat$setDataTransforms(
      names = list(
        "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
        "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
        "Stevens_2012_placebo.Sita_proximal",
        "Stevens_2012_placebo.Placebo_distal"
      ),
      xOffsets = list(2, 4, 5, 5, 6)
    )
  )

  expect_error(
    myCombDat$setDataTransforms(
      names = list(
        "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
        "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying"
      ),
      yOffsets = list(2, 5, 6)
    )
  )

  expect_error(
    myCombDat$setDataTransforms(
      names = list(
        "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
        "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying"
      ),
      xOffsets = list(2, 3, 6, 7),
      xScaleFactors = list(1.5, 2.4),
      yOffsets = c(4, 7, 8),
      yScaleFactors = c(1.1)
    )
  )

  myCombDat$setDataTransforms(
    names = names_ls,
    xOffsets = 2,
    yOffsets = 4,
    xScaleFactors = 1.5,
    yScaleFactors = 2.5
  )

  # check data transformations are correctly saved inside the object
  expect_equal(
    myCombDat$dataTransformations,
    structure(
      list(
        name = c(
          "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
          "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
          "Stevens_2012_placebo.Sita_proximal",
          "Stevens_2012_placebo.Placebo_distal"
        ),
        dataType = c("simulated", "simulated", "observed", "observed"),
        xOffsets = c(2, 2, 2, 2),
        xScaleFactors = c(1.5, 1.5, 1.5, 1.5),
        yOffsets = c(4, 4, 4, 4),
        yScaleFactors = c(2.5, 2.5, 2.5, 2.5)
      ),
      class = c("tbl_df", "tbl", "data.frame"),
      row.names = c(NA, -4L)
    )
  )

  dfTransformed <- myCombDat$toDataFrame()

  expect_equal(dfTransformed$xValues, (dfOriginal$xValues + 2) * 1.5)
  expect_equal(dfTransformed$yValues, (dfOriginal$yValues + 4) * 2.5)
  expect_equal(dfTransformed$yErrorValues, dfOriginal$yErrorValues * 2.5)

  # create object with datasets combined
  myCombDat2 <- DataCombined$new()
  myCombDat2$addDataSets(dataSet)
  myCombDat2$addSimulationResults(simResults)

  # should error with inappropriate arguments
  expect_error(myCombDat2$setDataTransforms(
    names = 2,
    xOffsets = "2"
  ))

  expect_error(myCombDat2$setDataTransforms(
    names = c(
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
      "Stevens_2012_placebo.Sita_proximal"
    ),
    xOffsets = c("2", 3),
    xScaleFactors = c("1.5", 2.4)
  ))

  myCombDat2$setDataTransforms(
    names = list(
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
      "Stevens_2012_placebo.Sita_proximal"
    ),
    # mix atomic vectors and lists to make sure that both work
    xOffsets = list(2, 3),
    xScaleFactors = list(1.5, 2.4),
    yOffsets = c(4, 7),
    yScaleFactors = c(1.1, 2.2)
  )

  # check the transformation values are accurately saved
  expect_equal(
    myCombDat2$dataTransformations,
    structure(
      list(
        name = c(
          "Stevens_2012_placebo.Sita_proximal",
          "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention"
        ),
        dataType = c("observed", "simulated"),
        xOffsets = c(3, 2),
        xScaleFactors = c(2.4, 1.5),
        yOffsets = c(7, 4),
        yScaleFactors = c(2.2, 1.1)
      ),
      class = c("tbl_df", "tbl", "data.frame"),
      row.names = c(NA, -2L)
    )
  )

  df <- myCombDat2$toDataFrame()

  # first level
  expect_equal(
    dplyr::filter(df, name == "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention")$xValues,
    (dplyr::filter(dfOriginal, name == "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention")$xValues + 2) * 1.5
  )
  expect_equal(
    dplyr::filter(df, name == "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention")$yValues,
    (dplyr::filter(dfOriginal, name == "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention")$yValues + 4) * 1.1
  )
  expect_equal(
    dplyr::filter(df, name == "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention")$yErrorValues,
    dplyr::filter(dfOriginal, name == "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention")$yErrorValues * 1.1
  )


  # second level
  expect_equal(
    dplyr::filter(df, name == "Stevens_2012_placebo.Sita_proximal")$xValues,
    (dplyr::filter(dfOriginal, name == "Stevens_2012_placebo.Sita_proximal")$xValues + 3) * 2.4
  )
  expect_equal(
    dplyr::filter(df, name == "Stevens_2012_placebo.Sita_proximal")$yValues,
    (dplyr::filter(dfOriginal, name == "Stevens_2012_placebo.Sita_proximal")$yValues + 7) * 2.2
  )
  expect_equal(
    dplyr::filter(df, name == "Stevens_2012_placebo.Sita_proximal")$yErrorValues,
    dplyr::filter(dfOriginal, name == "Stevens_2012_placebo.Sita_proximal")$yErrorValues * 2.2
  )

  expect_equal(
    head(df$xValues),
    c(
      9.52223539352417, 46.3355529785156, 80.7838302612305, 115.264205932617,
      154.742211914062, 186.81474609375
    )
  )

  expect_equal(
    head(df$yValues),
    c(
      179.262065076828, 148.917247438431, 126.917242193222, 107.193100237846,
      89.7448281288147, 75.3310338497162
    )
  )

  expect_equal(
    na.omit(df$yErrorValues),
    structure(c(
      10.6206896156073, 4.55172412097454, 6.82758618146181,
      3.79310343414545, 6.06896549463272, 5.31034480780363, 5.31034480780363,
      5.31034480780363, 6.06896549463272, 6.06896549463272, 4.55172412097454,
      0, 0
    ), na.action = structure(14:264, class = "omit"))
  )
})

# grouping works ---------------------------------

test_that("DataCombined works with data grouping", {
  skip_if_not_installed("R6")

  # load the simulation
  sim <- loadTestSimulation("MinimalModel")
  simResults <- importResultsFromCSV(
    simulation = sim,
    filePaths = getTestDataFilePath("Stevens_2012_placebo_indiv_results.csv")
  )

  # import observed data (will return a list of DataSet objects)
  dataSet <- loadDataSetsFromExcel(
    xlsFilePath = getTestDataFilePath("CompiledDataSetStevens2012.xlsx"),
    importerConfiguration = DataImporterConfiguration$new(getTestDataFilePath("ImporterConfiguration.xml"))
  )

  # create object with datasets combined
  myCombDat <- DataCombined$new()

  # expect error when grouping length and data type are inaccurate
  expect_error(myCombDat$addSimulationResults(simResults, groups = list("x", "y")))
  expect_error(myCombDat$addSimulationResults(simResults, groups = list(2, 4)))
  expect_error(myCombDat$addSimulationResults(simResults, groups = list(1, 2, 3, 4, 5)))
  expect_error(myCombDat$addDataSets(dataSet, groups = list("x")))
  expect_error(myCombDat$addDataSets(dataSet, groups = list(1)))
  expect_error(myCombDat$addDataSets(dataSet, groups = list(1, 2, 3, 4, 5, 6)))

  # proper grouping
  myCombDat$addSimulationResults(
    simResults,
    groups = list(NULL, NULL, "distal", "proximal", "total")
  )
  myCombDat$addDataSets(dataSet,
    groups = list("total", "total", "proximal", "proximal", "distal", "distal")
  )

  # order should matter
  myCombDat2 <- DataCombined$new()
  myCombDat2$addDataSets(dataSet,
    groups = list("total", "total", "proximal", "proximal", "distal", "distal")
  )
  myCombDat2$addSimulationResults(
    simResults,
    groups = list(NULL, NULL, "distal", "proximal", "total")
  )

  # check mapping
  dfMap <- myCombDat$groupMap
  dfMap2 <- myCombDat2$groupMap

  expect_equal(dim(dfMap), c(11L, 3L))
  expect_equal(dim(dfMap2), c(11L, 3L))

  # expect_equal(
  #   dfMap$group,
  #   c(
  #     "distal",
  #     "distal",
  #     "distal",
  #     "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
  #     "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
  #     "proximal",
  #     "proximal",
  #     "proximal",
  #     "total",
  #     "total",
  #     "total"
  #   )
  # )
  #
  # expect_equal(
  #   dfMap$name,
  #   c(
  #     "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
  #     "Stevens_2012_placebo.Placebo_distal",
  #     "Stevens_2012_placebo.Sita_dist",
  #     "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
  #     "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
  #     "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
  #     "Stevens_2012_placebo.Placebo_proximal",
  #     "Stevens_2012_placebo.Sita_proximal",
  #     "Organism|Lumen|Stomach|Metformin|Gastric retention",
  #     "Stevens_2012_placebo.Placebo_total",
  #     "Stevens_2012_placebo.Sita_total"
  #   )
  # )

  expect_equal(dfMap$group, dfMap2$group)
  expect_equal(dfMap$name, dfMap2$name)

  # check dataframe
  df <- myCombDat$toDataFrame()
  expect_s3_class(df, "data.frame")
  expect_equal(dim(df), c(1332L, 21L))
  expect_equal(
    names(df),
    c(
      "group", "dataType", "name", "IndividualId", "Group Id", "xValues",
      "xUnit", "xDimension", "yValues", "yUnit", "yDimension", "yErrorValues",
      "yErrorType", "yErrorUnit", "molWeight", "lloq", "Source", "Sheet",
      "Organ", "Compartment", "Molecule"
    )
  )

  df2 <- myCombDat2$toDataFrame()
  expect_s3_class(df2, "data.frame")
  expect_equal(dim(df2), c(1332L, 21L))
})


# sequential update - same values ---------------------------------

test_that("DataCombined works with sequential update - same values", {
  skip_if_not_installed("R6")

  # if the objects share the same datasets, then the one entered later will be used

  # load the simulation
  sim <- loadTestSimulation("MinimalModel")
  simResults <- importResultsFromCSV(
    simulation = sim,
    filePaths = getTestDataFilePath("Stevens_2012_placebo_indiv_results.csv")
  )

  # import observed data (will return a list of DataSet objects)
  dataSet <- loadDataSetsFromExcel(
    xlsFilePath = getTestDataFilePath("CompiledDataSetStevens2012.xlsx"),
    importerConfiguration = DataImporterConfiguration$new(getTestDataFilePath("ImporterConfiguration.xml"))
  )

  # create object with datasets combined
  myCombDat <- DataCombined$new()

  # add grouping
  myCombDat$addSimulationResults(
    simResults,
    groups = list(NULL, NULL, "distal", "proximal", "total")
  )
  myCombDat$addDataSets(
    dataSet,
    groups = list("total", "total", "proximal", "proximal", "distal", "distal")
  )

  # first dataframe
  df1 <- myCombDat$toDataFrame()

  # now add same object but with different groupings just to check the behavior
  myCombDat$addSimulationResults(
    simResults,
    groups = list("Dapagliflozin - emptying", "Dapagliflozin - retention", NULL, NULL, NULL)
  )
  myCombDat$addDataSets(
    dataSet,
    groups = list(NULL, NULL, NULL, NULL, "distal", "distal")
  )

  # second dataframe
  df2 <- myCombDat$toDataFrame()

  # should be twice the number of rows and same no. of columns
  expect_equal(nrow(df2), nrow(df1))
  expect_equal(length(df2), length(df1))
  expect_equal(head(df1$yValues), head(df2$yValues))

  # names should also be the same since they are the same objects
  expect_equal(unique(df1$name), unique(df2$name))

  # but groupings should now be different
  expect_equal(
    unique(df1$group),
    c(
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
      "distal",
      "proximal",
      "total"
    )
  )

  expect_equal(
    unique(df2$group),
    c(
      "Dapagliflozin - emptying",
      "Dapagliflozin - retention",
      "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
      "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
      "Organism|Lumen|Stomach|Metformin|Gastric retention",
      "Stevens_2012_placebo.Placebo_total",
      "Stevens_2012_placebo.Sita_total",
      "Stevens_2012_placebo.Placebo_proximal",
      "Stevens_2012_placebo.Sita_proximal",
      "distal"
    )
  )

  expect_equal(dim(myCombDat$groupMap), c(11L, 3L))

  # expect_equal(
  #   myCombDat$groupMap,
  #   structure(
  #     list(
  #       group = c(
  #         "Dapagliflozin - emptying",
  #         "Dapagliflozin - retention",
  #         "distal",
  #         "distal",
  #         "Organism|Lumen|Stomach|Metformin|Gastric retention",
  #         "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
  #         "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
  #         "Stevens_2012_placebo.Placebo_proximal",
  #         "Stevens_2012_placebo.Placebo_total",
  #         "Stevens_2012_placebo.Sita_proximal",
  #         "Stevens_2012_placebo.Sita_total"
  #       ),
  #       name = c(
  #         "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
  #         "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
  #         "Stevens_2012_placebo.Placebo_distal",
  #         "Stevens_2012_placebo.Sita_dist",
  #         "Organism|Lumen|Stomach|Metformin|Gastric retention",
  #         "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
  #         "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
  #         "Stevens_2012_placebo.Placebo_proximal",
  #         "Stevens_2012_placebo.Placebo_total",
  #         "Stevens_2012_placebo.Sita_proximal",
  #         "Stevens_2012_placebo.Sita_total"
  #       ),
  #       dataType = c(
  #         "simulated",
  #         "simulated",
  #         "observed",
  #         "observed",
  #         "simulated",
  #         "simulated",
  #         "simulated",
  #         "observed",
  #         "observed",
  #         "observed",
  #         "observed"
  #       )
  #     ),
  #     row.names = c(NA, -11L),
  #     class = c("tbl_df", "tbl", "data.frame")
  #   )
  # )
})


# sequential update - different values ---------------------------------

test_that("DataCombined works with sequential update - different values", {
  skip_if_not_installed("R6")

  # if the objects share the same datasets, then the one entered later will be used

  # load the simulation
  sim <- loadTestSimulation("MinimalModel")
  simResults <- importResultsFromCSV(
    simulation = sim,
    filePaths = getTestDataFilePath("Stevens_2012_placebo_indiv_results.csv")
  )

  # import observed data (will return a list of DataSet objects)
  dataSet <- loadDataSetsFromExcel(
    xlsFilePath = getTestDataFilePath("CompiledDataSetStevens2012.xlsx"),
    importerConfiguration = DataImporterConfiguration$new(getTestDataFilePath("ImporterConfiguration.xml"))
  )

  dataSet2 <- loadDataSetsFromExcel(
    xlsFilePath = getTestDataFilePath("CompiledDataSetStevens2012v2.xlsx"),
    importerConfiguration = DataImporterConfiguration$new(getTestDataFilePath("ImporterConfiguration.xml"))
  )

  # create object with datasets combined
  myCombDat <- DataCombined$new()

  myCombDat$addDataSets(dataSet)

  # let's focus only on the datasets which are going to be duplicated in the next update
  df1 <- myCombDat$toDataFrame()
  df1Filter <- dplyr::filter(
    df1,
    name %in% c("Stevens_2012_placebo.Placebo_total", "Stevens_2012_placebo.Sita_total")
  )

  # update object with another `DataSet` object which has common datasets
  myCombDat$addDataSets(dataSet2)

  df2 <- myCombDat$toDataFrame()
  df2Filter <- dplyr::filter(
    df2,
    name %in% c("Stevens_2012_placebo.Placebo_total", "Stevens_2012_placebo.Sita_total")
  )

  # check dimensions and values

  # they should be different since the new dataset that replaces the old one has
  # deliberately different values and rows
  expect_equal(dim(df1), c(77L, 20L))
  expect_equal(dim(df2), c(76L, 20L))
  expect_equal(dim(df1Filter), c(25L, 20L))
  expect_equal(dim(df2Filter), c(24L, 20L))

  expect_equal(
    head(df1Filter$xValues),
    c(
      0, 13.1722688674927, 29.4033622741699, 44.6470603942871, 73.079833984375,
      88.2731094360352
    )
  )

  expect_equal(
    head(df2Filter$xValues),
    c(
      0, 14.218487739563, 27.4033622741699, 43.6344528198242, 57.8403358459473,
      74.0672302246094
    )
  )
})

# population objects work ---------------------------------

test_that("DataCombined works with population", {
  skip_if(.Platform$OS.type != "windows")
  # ospsuite::initPKSim("C:\\Program Files\\Open Systems Pharmacology\\PK-Sim 10.0")

  # If no unit is specified, the default units are used. For "height" it is "dm",
  # for "weight" it is "kg", for "age" it is "year(s)".
  populationCharacteristics <- createPopulationCharacteristics(
    species = Species$Human,
    population = HumanPopulation$Asian_Tanaka_1996,
    numberOfIndividuals = 50,
    proportionOfFemales = 50,
    weightMin = 30,
    weightMax = 98,
    weightUnit = "kg",
    heightMin = NULL,
    heightMax = NULL,
    ageMin = 0,
    ageMax = 80,
    ageUnit = "year(s)"
  )

  # Create population from population characteristics
  result <- createPopulation(populationCharacteristics = populationCharacteristics)
  myPopulation <- result$population

  # Load simulation
  simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simFilePath)

  populationResults <- runSimulation(
    simulation = sim,
    population = myPopulation
  )

  myDataComb <- DataCombined$new()
  myDataComb$addSimulationResults(populationResults, individualIds = c(1, 8, 10, 44))
  df <- myDataComb$toDataFrame()

  expect_s3_class(df, "data.frame")
  expect_equal(dim(df), c(1964L, 9L))

  expect_equal(min(df$IndividualId), 1)
  expect_equal(max(df$IndividualId), 44)
  expect_equal(unique(df$yUnit), "µmol/l")
  expect_equal(unique(df$yDimension), "Concentration (molar)")
  expect_equal(unique(df$xUnit), "min")
})
