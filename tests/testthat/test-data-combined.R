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

  # NULLs now allowed
  expect_error(myCombDat$addDataSets(list(NULL)))
  expect_error(myCombDat$addSimulationResults(list(NULL)))
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
      "name", "group", "dataType", "xValues", "xDimension",
      "xUnit", "yValues", "yErrorValues", "yDimension", "yUnit", "yErrorType",
      "yErrorUnit", "molWeight", "lloq", "Source", "Sheet", "Organ",
      "Compartment", "Molecule", "Group Id"
    )
  )
  expect_equal(rep(NA_character_, length(df$group)), df$group)
  expect_equal(unique(df$name), names(dataSet)[[1]])

  # only SimulationResults input

  # selecting all paths
  myCombDat2 <- DataCombined$new()
  myCombDat2$addSimulationResults(simResults)

  # selecting a few paths
  myCombDat3 <- DataCombined$new()
  myCombDat3$addSimulationResults(
    simResults,
    quantitiesOrPaths = c(
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
      "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
      "Organism|Lumen|Stomach|Metformin|Gastric retention"
    ),
    names = c("x", NA_character_, "y"),
    groups = c("a", NA_character_, "b")
  )

  expect_equal(dplyr::filter(myCombDat3$groupMap, group == "a")$name[[1]], "x")
  expect_equal(dplyr::filter(myCombDat3$groupMap, group == "b")$name[[1]], "y")
  expect_equal(
    dplyr::filter(myCombDat3$groupMap, !group %in% c("a", "b"))$name[[1]],
    "Organism|Lumen|Stomach|Metformin|Gastric retention distal"
  )

  expect_true(R6::is.R6(myCombDat2))
  expect_false(R6::is.R6Class(myCombDat2))

  # checking dataframe methods
  df2 <- myCombDat2$toDataFrame()
  expect_s3_class(df2, "data.frame")
  expect_equal(dim(df2), c(1255L, 9L))

  expect_equal(
    names(df2),
    c(
      "name", "group", "dataType", "xValues", "xUnit", "yValues",
      "yUnit", "yDimension", "IndividualId"
    )
  )
  expect_equal(unique(df2$name), sort(simResults$allQuantityPaths))

  expect_equal(
    as.character(na.omit(unique(df2$name))),
    c(
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
      "Organism|Lumen|Stomach|Metformin|Gastric retention",
      "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
      "Organism|Lumen|Stomach|Metformin|Gastric retention proximal"
    )
  )

  expect_equal(rep(NA_character_, length(df2$group)), df2$group)
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

  # with list and name inputs ----------------------------

  # create a new instance of the object
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
      "name", "group", "dataType", "xValues", "xUnit", "xDimension",
      "yValues", "yUnit", "yDimension", "yErrorValues", "yErrorType", "yErrorUnit",
      "IndividualId", "molWeight", "lloq", "Source", "Sheet",
      "Organ", "Compartment", "Molecule", "Group Id"
    )
  )

  # no grouping, so all NA
  expect_equal(rep(NA_character_, length(df$group)), df$group)

  # these should be the same since it's the same dataset with a different name
  expect_equal(
    dplyr::filter(df, name == "x")$yValues,
    simulationResultsToDataFrame(
      simResults,
      quantitiesOrPaths = "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention"
    )$simulationValues
  )

  expect_equal(
    dplyr::filter(df, name == "y")$yValues,
    simulationResultsToDataFrame(
      simResults,
      quantitiesOrPaths = "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying"
    )$simulationValues
  )

  expect_equal(
    dplyr::filter(df, name == "z")$yValues,
    simulationResultsToDataFrame(
      simResults,
      quantitiesOrPaths = "Organism|Lumen|Stomach|Metformin|Gastric retention"
    )$simulationValues
  )

  # read-only
  expect_error(myCombDat$dataSets(dataSet))
  expect_error(myCombDat$simulationResults(simResults))

  # not specified, so NA
  expect_equal(
    myCombDat$groupMap$group,
    rep(NA_character_, length(myCombDat$groupMap$group))
  )

  # with DataSet input ----------------------------

  # create a new instance of the object
  myCombDat2 <- DataCombined$new()
  myCombDat2$addSimulationResults(simResults)
  myCombDat2$addDataSets(dataSet[[1]])

  expect_true(R6::is.R6(myCombDat2))
  expect_false(R6::is.R6Class(myCombDat2))

  # checking dataframe methods
  df2 <- myCombDat2$toDataFrame()
  expect_s3_class(df2, "data.frame")
  expect_equal(dim(df2), c(1267L, 21L))

  # not specified, so NA
  expect_equal(
    myCombDat$groupMap$group,
    rep(NA_character_, length(myCombDat$groupMap$group))
  )

  expect_equal(
    as.character(unique(df2$name)),
    c(
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
      "Organism|Lumen|Stomach|Metformin|Gastric retention",
      "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
      "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
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
      "Organism|Lumen|Stomach|Metformin|Gastric retention",
      "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
      "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
      "Stevens_2012_placebo.Placebo_total",
      "Stevens_2012_placebo.Sita_total"
    )
  )

  # not specified, so NA
  expect_equal(
    myCombDat$groupMap$group,
    rep(NA_character_, length(myCombDat$groupMap$group))
  )

  # printing objects works
  expect_output(print(myCombDat))
  expect_output(print(myCombDat2))
  expect_output(print(myCombDat3))
  expect_output(print(myCombDat4))
  expect_output(print(myCombDat5))
})

# same data order with or without `names` argument -------------

test_that("dataCombined - same data order with or without `names` argument", {
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

  # create a new instance of the object
  myCombDat <- DataCombined$new()
  myCombDat2 <- DataCombined$new()

  # add a list of datasets without names in the container list
  # don't specify names argument for one, while do for the other
  myCombDat$addDataSets(list(dataSet[[1]], dataSet[[2]], dataSet[[3]]))
  myCombDat2$addDataSets(list(dataSet[[1]], dataSet[[2]], dataSet[[3]]),
    names = list("x", "y", NULL)
  )

  # extract dataframes
  df1 <- myCombDat$toDataFrame()
  df2 <- myCombDat2$toDataFrame()

  # except for names column, everything should look the same across two objects
  expect_equal(nrow(df1), nrow(df2))
  expect_equal(length(df1), length(df2))
  expect_equal(df1$xValues, df2$xValues)
  expect_equal(df1$yValues, df2$yValues)

  expect_equal(
    unique(df1$name),
    c(
      "Stevens_2012_placebo.Placebo_proximal",
      "Stevens_2012_placebo.Placebo_total",
      "Stevens_2012_placebo.Sita_total"
    )
  )

  expect_equal(
    unique(df2$name),
    c("Stevens_2012_placebo.Placebo_proximal", "x", "y")
  )


  # create a new instance of the object
  myCombDat3 <- DataCombined$new()
  myCombDat4 <- DataCombined$new()

  # both of these should provide the same result
  myCombDat3$addDataSets(list(dataSet[[1]], dataSet[[2]], dataSet[[3]]))
  myCombDat4$addDataSets(c(dataSet[[1]], dataSet[[2]], dataSet[[3]]))

  expect_equal(myCombDat3$toDataFrame(), myCombDat4$toDataFrame())
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

  # create a new instance of the object
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
    myCombDat$setDataTransformations(
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
    myCombDat$setDataTransformations(
      names = list(
        "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
        "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying"
      ),
      yOffsets = list(2, 5, 6)
    )
  )

  expect_error(
    myCombDat$setDataTransformations(
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

  myCombDat$setDataTransformations(
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
          "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
          "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
          "Organism|Lumen|Stomach|Metformin|Gastric retention",
          "Stevens_2012_placebo.Placebo_total",
          "Stevens_2012_placebo.Sita_total",
          "Stevens_2012_placebo.Placebo_proximal",
          "Stevens_2012_placebo.Sita_proximal",
          "Stevens_2012_placebo.Placebo_distal",
          "Stevens_2012_placebo.Sita_dist"
        ),
        xOffsets = c(2, 2, 0, 0, 0, 0, 0, 0, 2, 2, 0),
        yOffsets = c(4, 4, 0, 0, 0, 0, 0, 0, 4, 4, 0),
        xScaleFactors = c(1.5, 1.5, 1, 1, 1, 1, 1, 1, 1.5, 1.5, 1),
        yScaleFactors = c(2.5, 2.5, 1, 1, 1, 1, 1, 1, 2.5, 2.5, 1)
      ),
      class = c("tbl_df", "tbl", "data.frame"),
      row.names = c(NA, -11L)
    )
  )

  dfTransformed <- myCombDat$toDataFrame()
  dfTransformed <- dplyr::filter(dfTransformed, name %in% names_ls)

  expect_equal(dfTransformed$xValues, (dfOriginal$xValues + 2) * 1.5)
  expect_equal(dfTransformed$yValues, (dfOriginal$yValues + 4) * 2.5)
  expect_equal(dfTransformed$yErrorValues, dfOriginal$yErrorValues * 2.5)

  # create a new instance of the object
  myCombDat2 <- DataCombined$new()
  myCombDat2$addDataSets(dataSet)
  myCombDat2$addSimulationResults(simResults)

  # should error with inappropriate arguments
  expect_error(myCombDat2$setDataTransformations(
    names = 2,
    xOffsets = "2"
  ))

  expect_error(myCombDat2$setDataTransformations(
    names = c(
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
      "Stevens_2012_placebo.Sita_proximal"
    ),
    xOffsets = c("2", 3),
    xScaleFactors = c("1.5", 2.4)
  ))

  myCombDat2$setDataTransformations(
    names = list(
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
      "Stevens_2012_placebo.Sita_proximal"
    ),
    # mix atomic vectors and lists to make sure that both work
    xOffsets = list(2, 3),
    yOffsets = c(4, 7),
    xScaleFactors = list(1.5, 2.4),
    yScaleFactors = c(1.1, 2.2)
  )

  # check the transformation values are accurately saved
  expect_equal(
    myCombDat2$dataTransformations,
    structure(
      list(
        name = c(
          "Stevens_2012_placebo.Placebo_total",
          "Stevens_2012_placebo.Sita_total",
          "Stevens_2012_placebo.Placebo_proximal",
          "Stevens_2012_placebo.Sita_proximal",
          "Stevens_2012_placebo.Placebo_distal",
          "Stevens_2012_placebo.Sita_dist",
          "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
          "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
          "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
          "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
          "Organism|Lumen|Stomach|Metformin|Gastric retention"
        ),
        xOffsets = c(0, 0, 0, 3, 0, 0, 0, 2, 0, 0, 0),
        yOffsets = c(0, 0, 0, 7, 0, 0, 0, 4, 0, 0, 0),
        xScaleFactors = c(1, 1, 1, 2.4, 1, 1, 1, 1.5, 1, 1, 1),
        yScaleFactors = c(1, 1, 1, 2.2, 1, 1, 1, 1.1, 1, 1, 1)
      ),
      class = c("tbl_df", "tbl", "data.frame"),
      row.names = c(NA, -11L)
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

  # each call to set transformations resets the previous parameters for the same
  # dataset
  myCombDat3 <- DataCombined$new()

  myCombDat3$addSimulationResults(simResults, quantitiesOrPaths = "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying")
  myCombDat3$setDataTransformations()
  df1 <- myCombDat3$dataTransformations

  myCombDat3$setDataTransformations(xOffsets = 3, yScaleFactors = 4)
  df2 <- myCombDat3$dataTransformations

  myCombDat3$setDataTransformations()
  df3 <- myCombDat3$dataTransformations

  expect_equal(df1, df3)
  expect_equal(df2$xOffsets, 3)
  expect_equal(df2$yScaleFactors, 4)

  # make sure messy inputs for transformations don't cause any problems
  myCombDat4 <- DataCombined$new()
  myCombDat4$addDataSets(dataSet)
  myCombDat4$addSimulationResults(simResults)

  myCombDat4$setDataTransformations(
    names = names_ls,
    xOffsets = list(NULL, NA, NA_character_, NULL),
    yOffsets = c(NA, NA_real_, NA, NaN),
    xScaleFactors = c(NA, NA_integer_, NA, NA),
    yScaleFactors = list(NULL, NaN, NA, NULL)
  )

  expect_equal(
    myCombDat4$dataTransformations,
    structure(
      list(
        name = c(
          "Stevens_2012_placebo.Placebo_total",
          "Stevens_2012_placebo.Sita_total",
          "Stevens_2012_placebo.Placebo_proximal",
          "Stevens_2012_placebo.Sita_proximal",
          "Stevens_2012_placebo.Placebo_distal",
          "Stevens_2012_placebo.Sita_dist",
          "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
          "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
          "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
          "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
          "Organism|Lumen|Stomach|Metformin|Gastric retention"
        ),
        xOffsets = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        yOffsets = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        xScaleFactors = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
        yScaleFactors = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
      ),
      class = c("tbl_df", "tbl", "data.frame"),
      row.names = c(NA, -11L)
    )
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

  # create a new instance of the object
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
    groups = list(NULL, NA_complex_, "distal", "proximal", "total")
  )

  # check mapping
  dfMap <- myCombDat$groupMap
  dfMap2 <- myCombDat2$groupMap

  expect_equal(dim(dfMap), c(11L, 3L))
  expect_equal(dim(dfMap2), c(11L, 3L))

  expect_equal(
    dplyr::filter(dfMap, group == "distal", dataType == "simulated")$name,
    "Organism|Lumen|Stomach|Metformin|Gastric retention distal"
  )

  expect_equal(
    dplyr::filter(dfMap, group == "distal", dataType == "observed")$name,
    c("Stevens_2012_placebo.Placebo_distal", "Stevens_2012_placebo.Sita_dist")
  )

  expect_equal(
    dplyr::filter(dfMap, group == "proximal", dataType == "simulated")$name,
    "Organism|Lumen|Stomach|Metformin|Gastric retention proximal"
  )

  expect_equal(
    dplyr::filter(dfMap, group == "proximal", dataType == "observed")$name,
    c("Stevens_2012_placebo.Placebo_proximal", "Stevens_2012_placebo.Sita_proximal")
  )

  expect_equal(
    dplyr::filter(dfMap, group == "total", dataType == "simulated")$name,
    "Organism|Lumen|Stomach|Metformin|Gastric retention"
  )

  expect_equal(
    dplyr::filter(dfMap, group == "total", dataType == "observed")$name,
    c("Stevens_2012_placebo.Placebo_total", "Stevens_2012_placebo.Sita_total")
  )

  expect_equal(dfMap$group, dfMap2$group)
  expect_equal(dfMap$name, dfMap2$name)

  # check dataframe
  df <- myCombDat$toDataFrame()
  expect_s3_class(df, "data.frame")
  expect_equal(dim(df), c(1332L, 21L))
  expect_equal(
    names(df),
    c(
      "name", "group", "dataType", "xValues", "xUnit", "xDimension",
      "yValues", "yUnit", "yDimension", "yErrorValues", "yErrorType", "yErrorUnit",
      "IndividualId", "molWeight", "lloq", "Source", "Sheet",
      "Organ", "Compartment", "Molecule", "Group Id"
    )
  )

  df2 <- myCombDat2$toDataFrame()
  expect_s3_class(df2, "data.frame")
  expect_equal(dim(df2), c(1332L, 21L))

  # single dataset can also be added with a group
  myCombDat3 <- DataCombined$new()
  myCombDat3$addDataSets(dataSet[[1]], groups = "x")
  expect_equal(myCombDat3$groupMap$group[[1]], "x")
  expect_equal(myCombDat3$groupMap$name[[1]], "Stevens_2012_placebo.Placebo_total")

  # entering NA doesn't cause any problems
  myCombDat4 <- DataCombined$new()
  myCombDat4$addDataSets(dataSet[[1]], groups = NA_character_)
  expect_equal(
    myCombDat4$groupMap$group[[1]],
    rep(NA_character_, length(myCombDat4$groupMap$group[[1]]))
  )
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

  # create a new instance of the object
  myCombDat <- DataCombined$new()

  # add grouping
  myCombDat$addSimulationResults(
    simResults,
    groups = list(NULL, NaN, "distal", "proximal", "total")
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
    groups = list("Dapagliflozin - emptying", "Dapagliflozin - retention", NaN, NULL, NA_real_)
  )
  myCombDat$addDataSets(
    dataSet,
    groups = list(NULL, NA_integer_, NA, NaN, "distal", "distal")
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
    c(NA_character_, "total", "distal", "proximal")
  )

  expect_equal(
    unique(df2$group),
    c("Dapagliflozin - emptying", "Dapagliflozin - retention", NA_character_, "distal")
  )

  # extract group map into a dataframe
  dfMap <- myCombDat$groupMap

  expect_equal(dim(dfMap), c(11L, 3L))

  expect_equal(
    dplyr::filter(dfMap, group == "Dapagliflozin - emptying")$name,
    "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying"
  )

  expect_equal(
    dplyr::filter(dfMap, group == "Dapagliflozin - retention")$name,
    "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention"
  )

  expect_equal(
    dplyr::filter(dfMap, group == "distal")$name,
    c(
      "Stevens_2012_placebo.Placebo_distal",
      "Stevens_2012_placebo.Sita_dist"
    )
  )

  expect_equal(
    dplyr::filter(dfMap, is.na(group))$name,
    c(
      "Organism|Lumen|Stomach|Metformin|Gastric retention",
      "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
      "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
      "Stevens_2012_placebo.Placebo_proximal",
      "Stevens_2012_placebo.Placebo_total",
      "Stevens_2012_placebo.Sita_proximal",
      "Stevens_2012_placebo.Sita_total"
    )
  )
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

  # create a new instance of the object
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


# edge cases ---------------------------------

test_that("DataCombined works with edge cases", {
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

  ## groups name same as dataset name ---------------------------

  # create a new instance of the object
  myCombDat <- DataCombined$new()

  # dataset name is "Stevens_2012_placebo.Placebo_total" but no group
  myCombDat$addDataSets(dataSet[[1]])

  # dataset name is "Stevens_2012_placebo.Sita_total" but no grouping assigned is
  # same as dataset name entered before
  myCombDat$addDataSets(dataSet[[2]], groups = "Stevens_2012_placebo.Placebo_total")

  # they shouldn't be collapsed together in the same group
  expect_equal(
    myCombDat$groupMap$group,
    c("Stevens_2012_placebo.Placebo_total", NA_character_)
  )

  # list/vector and NULL/NA ---------------------------

  # create a new instance of the object
  myCombDat2 <- DataCombined$new()
  myCombDat3 <- DataCombined$new()

  # make sure the default logical NA doesn't create any issues
  myCombDat2$addDataSets(
    list(dataSet[[1]], dataSet[[2]], dataSet[[3]], dataSet[[4]]),
    names = list(NULL, "x", NA_character_, "y"),
    groups = c("a", NA_character_, "b", NA)
  )
  myCombDat3$addDataSets(
    list(dataSet[[1]], dataSet[[2]], dataSet[[3]], dataSet[[4]]),
    names = c(NA_character_, "x", NA, "y"),
    groups = list("a", NULL, "b", NA_character_)
  )

  myCombDat2$setDataTransformations(
    names = list(
      "Stevens_2012_placebo.Placebo_total",
      "x",
      "Stevens_2012_placebo.Placebo_proximal",
      "y"
    ),
    xOffsets = list(1.2, 2, 2.3, 4.8),
    yOffsets = c(3.1, 4.4, 3.5, 6.1),
    xScaleFactors = c(1.2, 2, 2.3, 4.8),
    yScaleFactors = list(3.1, 4.4, 3.5, 6.1)
  )
  myCombDat3$setDataTransformations(
    xOffsets = list(1.2, 2, 2.3, 4.8),
    yOffsets = c(3.1, 4.4, 3.5, 6.1),
    xScaleFactors = c(1.2, 2, 2.3, 4.8),
    yScaleFactors = list(3.1, 4.4, 3.5, 6.1)
  )

  expect_equal(myCombDat2$groupMap, myCombDat3$groupMap)
  expect_equal(myCombDat2$dataTransformations, myCombDat3$dataTransformations)
})
