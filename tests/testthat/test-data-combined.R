# data to be used ---------------------------------------

# `loadDataSetsFromExcel()` does not work for non-Windows platforms
if (.Platform$OS.type == "windows") {

  # load the simulation
  sim <- loadTestSimulation("MinimalModel")
  simResults <- importResultsFromCSV(
    simulation = sim,
    filePaths = getTestDataFilePath("Stevens_2012_placebo_indiv_results.csv")
  )

  # import observed data (will return a list of DataSet objects)
  dataSet <- loadDataSetsFromExcel(
    xlsFilePath = getTestDataFilePath("CompiledDataSetStevens2012.xlsx"),
    importerConfiguration = loadDataImporterConfiguration(getTestDataFilePath("ImporterConfiguration.xml"))
  )

  # same as dataSet, but with deliberately changed entries for testing
  dataSet2 <- loadDataSetsFromExcel(
    xlsFilePath = getTestDataFilePath("CompiledDataSetStevens2012v2.xlsx"),
    importerConfiguration = loadDataImporterConfiguration(getTestDataFilePath("ImporterConfiguration.xml"))
  )

  # active bindings ---------------------------------------

  test_that("active bindings should all be NULL for empty initialization", {
    myCombDat <- DataCombined$new()

    expect_null(myCombDat$groupMap)
    expect_null(myCombDat$names)
    expect_null(myCombDat$toDataFrame())
    expect_output(print(myCombDat), "DataCombined:")
  })

  test_that("active bindings are read-only", {
    myCombDat <- DataCombined$new()

    expect_error(
      myCombDat$groupMap <- "x",
      messages$errorPropertyReadOnly("groupMap"),
      fixed = TRUE
    )

    expect_error(
      myCombDat$names <- "x",
      messages$errorPropertyReadOnly("names"),
      fixed = TRUE
    )

    expect_error(
      myCombDat$dataTransformations <- "x",
      messages$errorPropertyReadOnly("dataTransformations"),
      fixed = TRUE
    )
  })

  test_that("add* methods error if anything but expected data types are entered", {
    myCombDat <- DataCombined$new()

    expect_error(
      myCombDat$addSimulationResults(list("x", "y")),
      "argument 'simulationResults' is of type 'list', but expected 'SimulationResults'"
    )

    expect_error(
      myCombDat$addDataSets(list(1, 2)),
      "argument 'dataSets' is of type 'list', but expected 'DataSet'"
    )
  })

  # only `DataSet` ---------------------------------------

  test_that("data transformations work as expected when only `DataSet` is provided", {
    myCombDat <- DataCombined$new()
    myCombDat$addDataSets(dataSet[[1]])

    # data transformations should already be setup with the defaults
    expect_equal(
      myCombDat$dataTransformations,
      structure(
        list(
          name = "Stevens_2012_placebo.Placebo_total",
          xOffsets = 0,
          yOffsets = 0,
          xScaleFactors = 1,
          yScaleFactors = 1
        ),
        class = c("tbl_df", "tbl", "data.frame"),
        row.names = c(NA, -1L)
      )
    )
  })

  test_that("data frame dimensions are as expected when only `DataSet` is provided", {
    myCombDat <- DataCombined$new()
    myCombDat$addDataSets(dataSet[[1]])
    df <- myCombDat$toDataFrame()

    expect_equal(dim(df), c(12L, 17L))
  })

  test_that("data frame column and dataset names are as expected when only `DataSet` is provided", {
    myCombDat <- DataCombined$new()
    myCombDat$addDataSets(dataSet[[1]])
    df <- myCombDat$toDataFrame()

    expect_equal(
      names(df),
      c(
        "name", "group", "dataType", "xValues", "xUnit", "xDimension",
        "yValues", "yUnit", "yDimension", "yErrorValues", "yErrorType",
        "yErrorUnit", "molWeight", "lloq", "Source", "Sheet", "Group Id"
      )
    )

    expect_equal(unique(df$name), names(dataSet)[[1]])
  })

  # only `SimulationResults` ---------------------------------------

  test_that("data transformations work as expected when only `SimulationResults` is provided", {
    myCombDat <- DataCombined$new()
    myCombDat$addSimulationResults(simResults)

    # data transformations should already be setup with the defaults
    expect_equal(
      myCombDat$dataTransformations,
      structure(
        list(
          name = c(
            "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
            "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
            "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
            "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
            "Organism|Lumen|Stomach|Metformin|Gastric retention"
          ),
          xOffsets = c(0, 0, 0, 0, 0),
          yOffsets = c(0, 0, 0, 0, 0),
          xScaleFactors = c(1, 1, 1, 1, 1),
          yScaleFactors = c(1, 1, 1, 1, 1)
        ),
        class = c("tbl_df", "tbl", "data.frame"),
        row.names = c(NA, -5L)
      )
    )
  })

  test_that("data frame dimensions are as expected when only `SimulationResults` is provided", {
    myCombDat <- DataCombined$new()
    myCombDat$addSimulationResults(simResults)
    df <- myCombDat$toDataFrame()

    expect_equal(dim(df), c(1255L, 12L))
  })

  test_that("data frame column names and datset names are as expected when only `SimulationResults` is provided", {
    myCombDat <- DataCombined$new()
    myCombDat$addSimulationResults(simResults)
    df <- myCombDat$toDataFrame()

    expect_equal(
      names(df),
      c(
        "name", "group", "dataType", "xValues", "xUnit", "xDimension",
        "yValues", "yUnit", "yDimension", "yErrorValues", "IndividualId", "molWeight"
      )
    )
    expect_equal(unique(df$name), sort(simResults$allQuantityPaths))
    expect_equal(
      as.character(na.omit(unique(df$name))),
      c(
        "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
        "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
        "Organism|Lumen|Stomach|Metformin|Gastric retention",
        "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
        "Organism|Lumen|Stomach|Metformin|Gastric retention proximal"
      )
    )
  })

  test_that("data frame molecular weight column values are as expected", {
    myCombDat <- DataCombined$new()
    myCombDat$addSimulationResults(simResults)
    df <- myCombDat$toDataFrame()

    expect_equal(unique(df$molWeight), c(408.8730, 129.1636))
  })

  # grouping specification ---------------------------------------

  test_that("with no grouping specified, group column in data frame is `NA`", {
    myCombDat <- DataCombined$new()
    myCombDat$addDataSets(dataSet[[1]])
    df <- myCombDat$toDataFrame()

    expect_equal(rep(NA_character_, length(df$group)), df$group)
  })


  test_that("grouping specification fails when there are no datasets present", {
    myCombDat <- DataCombined$new()

    expect_error(
      myCombDat$setGroups(names = c("x", "y"), groups = c("a", "b")),
      "There are currently no datasets to be grouped."
    )
  })

  test_that("grouping specification fails when arguments are empty", {
    myCombDat <- DataCombined$new()
    myCombDat$addSimulationResults(simResults)

    expect_error(
      myCombDat$setGroups(names = character(), groups = c("a", "b")),
      "argument 'arg' is empty"
    )

    expect_error(
      myCombDat$setGroups(names = c("a", "b"), groups = character()),
      "argument 'arg' is empty"
    )
  })

  test_that("setting groups fails when arguments are not of `character` type", {
    myCombDat <- DataCombined$new()
    myCombDat$addDataSets(dataSet)

    # TODO: Include full error messages in the following tests once
    # https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils/issues/77
    # has been resolved

    x <- list(2, 4)

    expect_error(
      myCombDat$setGroups(x, list("a", "b")),
      "but expected 'character'"
    )

    expect_error(
      myCombDat$setGroups(list(names = "a", "b"), x),
      "but expected 'character'"
    )
  })

  test_that("setting groups fails when group specification is same for multiple datsets", {
    myCombDat <- DataCombined$new()
    myCombDat$addDataSets(dataSet)

    expect_error(
      myCombDat$setGroups(
        names = c("Stevens_2012_placebo.Placebo_distal", "Stevens_2012_placebo.Placebo_distal"),
        groups = c("a", "b")
      ),
      "Object has duplicated values; only unique values are allowed."
    )
  })

  test_that("assigning groups produces a message if dataset name is not found", {
    myCombDat <- DataCombined$new()
    myCombDat$addDataSets(dataSet[[1]])

    expect_message(
      myCombDat$setGroups(
        names = list("Stevens_2012_placebo.Placebo_total", "x", "y"),
        groups = list("m", "a", "b")
      ),
      "Following datasets were specified to be grouped but not found:
x
y
",
      fixed = TRUE
    )
  })

  test_that("assigned group can be removed using `NA` or `NULL`", {
    myCombDat <- DataCombined$new()
    myCombDat$addDataSets(dataSet[[1]])

    myCombDat$setGroups(names = "Stevens_2012_placebo.Placebo_total", groups = "m")
    myCombDat$setGroups(names = "Stevens_2012_placebo.Placebo_total", groups = NA_real_)
    expect_equal(myCombDat$groupMap$group, NA_character_)

    myCombDat$setGroups(names = "Stevens_2012_placebo.Placebo_total", groups = "m")
    myCombDat$setGroups(names = "Stevens_2012_placebo.Placebo_total", groups = list(NULL))
    expect_equal(myCombDat$groupMap$group, NA_character_)
  })

  test_that("`$removeGroupAssignment()` produces error if there are no datasets", {
    myCombDat <- DataCombined$new()

    expect_error(
      myCombDat$removeGroupAssignment(names = "Stevens_2012_placebo.Placebo_total"),
      "There are currently no datasets. You can add them with `$addDataSets()` and/or `$addSimulationResults()` methods.",
      fixed = TRUE
    )
  })

  test_that("existing grouping can be removed using `$removeGroupAssignment()` method", {
    myCombDat <- DataCombined$new()
    myCombDat$addDataSets(dataSet[[1]])

    myCombDat$setGroups(names = "Stevens_2012_placebo.Placebo_total", groups = "m")
    myCombDat$removeGroupAssignment(names = "Stevens_2012_placebo.Placebo_total")
    expect_equal(myCombDat$groupMap$group, NA_character_)

    myCombDat$setGroups(names = "Stevens_2012_placebo.Placebo_total", groups = "m")
    myCombDat$removeGroupAssignment(names = list("Stevens_2012_placebo.Placebo_total"))
    expect_equal(myCombDat$groupMap$group, NA_character_)
  })

  test_that("`$removeGroupAssignment()` produces a message if dataset names are not found", {
    myCombDat <- DataCombined$new()
    myCombDat$addDataSets(dataSet[[1]])
    myCombDat$setGroups(names = "Stevens_2012_placebo.Placebo_total", groups = "m")

    expect_message(
      myCombDat$removeGroupAssignment(names = list("Stevens_2012_placebo.Placebo_total", "x", "y")),
      "Following datasets were specified to be grouped but not found:
x
y
",
      fixed = TRUE
    )
  })

  test_that("`$removeGroupAssignment()` produces error if names are not unique", {
    myCombDat <- DataCombined$new()
    myCombDat$addDataSets(dataSet[[1]])

    myCombDat$setGroups(names = "Stevens_2012_placebo.Placebo_total", groups = "m")
    expect_error(
      myCombDat$removeGroupAssignment(
        names = c(
          "Stevens_2012_placebo.Placebo_total",
          "Stevens_2012_placebo.Placebo_total"
        )
      ),
      "Object has duplicated values; only unique values are allowed.",
      fixed = TRUE
    )
  })

  test_that("setting groups with atomic vector or list shouldn't make a difference", {
    myCombDat <- DataCombined$new()
    myCombDat$addSimulationResults(simResults)
    myCombDat$setGroups(
      names = c(
        "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
        "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention"
      ),
      groups = c("x", "y")
    )

    myCombDat2 <- DataCombined$new()
    myCombDat2$addSimulationResults(simResults)
    myCombDat2$setGroups(
      names = list(
        "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
        "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention"
      ),
      groups = list("x", "y")
    )

    expect_equal(myCombDat$groupMap, myCombDat2$groupMap)
    expect_equal(myCombDat$toDataFrame(), myCombDat2$toDataFrame())
  })


  test_that("specifying groupings and new names for only few paths in `SimulationResults` works as expected", {
    myCombDat <- DataCombined$new()
    myCombDat$addSimulationResults(
      simResults,
      quantitiesOrPaths = c(
        "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
        "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
        "Organism|Lumen|Stomach|Metformin|Gastric retention"
      ),
      names = c("x", NA_character_, "y")
    )
    myCombDat$setGroups(names = list("x", "y"), groups = list("a", "b"))

    expect_equal(dplyr::filter(myCombDat$groupMap, group == "a")$name[[1]], "x")
    expect_equal(dplyr::filter(myCombDat$groupMap, group == "b")$name[[1]], "y")
    expect_equal(
      dplyr::filter(myCombDat$groupMap, !group %in% c("a", "b"))$name[[1]],
      "Organism|Lumen|Stomach|Metformin|Gastric retention distal"
    )
  })

  # both `DataSet` and `SimulationResults` ---------------------------------------

  test_that("data transformations as expected when both `DataSet` and `SimulationResults` provided", {
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

    expect_equal(
      myCombDat$dataTransformations,
      structure(
        list(
          name = c(
            "x",
            "y",
            "z",
            "a",
            "Stevens_2012_placebo.Sita_total",
            "b",
            "Stevens_2012_placebo.Sita_proximal",
            "c",
            "Stevens_2012_placebo.Sita_dist"
          ),
          xOffsets = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
          yOffsets = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
          xScaleFactors = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
          yScaleFactors = c(1, 1, 1, 1, 1, 1, 1, 1, 1)
        ),
        class = c("tbl_df", "tbl", "data.frame"),
        row.names = c(NA, -9L)
      )
    )
  })

  test_that("data frame dimensions as expected when both `DataSet` and `SimulationResults` provided", {
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

    myCombDat$addDataSets(
      dataSet,
      names = list("a", NULL, "b", NULL, "c", NULL)
    )

    df <- myCombDat$toDataFrame()
    expect_equal(dim(df), c(830L, 18L))
  })

  test_that("data frame column names are as expected when both `DataSet` and `SimulationResults` provided", {
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

    myCombDat$addDataSets(
      dataSet,
      names = list("a", NULL, "b", NULL, "c", NULL)
    )

    df <- myCombDat$toDataFrame()

    expect_equal(
      names(df),
      c(
        "name", "group", "dataType", "xValues", "xUnit", "xDimension",
        "yValues", "yUnit", "yDimension", "yErrorValues", "yErrorType", "yErrorUnit",
        "IndividualId", "molWeight", "lloq", "Source", "Sheet", "Group Id"
      )
    )
  })

  test_that("data frames for selected output paths match with outputs from `simulationResultsToDataFrame()` function", {
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

    myCombDat$addDataSets(
      dataSet,
      names = list("a", NULL, "b", NULL, "c", NULL)
    )

    df <- myCombDat$toDataFrame()

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
  })

  # order and renaming -----------------------------

  test_that("renaming only a single dataset works", {
    myCombDat <- DataCombined$new()
    myCombDat$addSimulationResults(
      simResults,
      quantitiesOrPaths = c("Organism|Lumen|Stomach|Dapagliflozin|Gastric retention"),
      names = list("m")
    )
    expect_equal(myCombDat$names, "m")
  })

  test_that("order in which objects are entered should not matter and method chaining works", {
    myCombDat <- DataCombined$new()

    df1 <- myCombDat$addSimulationResults(simResults)$addDataSets(dataSet[[1]])$toDataFrame()
    df2 <- myCombDat$addDataSets(dataSet[[1]])$addSimulationResults(simResults)$toDataFrame()

    expect_equal(df1, df2)
  })

  test_that("data order with or without `names` argument should be same", {
    myCombDat <- DataCombined$new()
    myCombDat2 <- DataCombined$new()

    # add a list of datasets without names in the container list
    # don't specify names argument for one, while do for the other
    myCombDat$addDataSets(list(dataSet[[1]], dataSet[[2]], dataSet[[3]]))
    myCombDat2$addDataSets(list(dataSet[[1]], dataSet[[2]], dataSet[[3]]),
      names = list("x", "y", NULL)
    )

    # extract data frames
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
  })

  test_that("data frame should be same when objects are entered either as a list or a vector", {
    myCombDat1 <- DataCombined$new()
    myCombDat2 <- DataCombined$new()

    # both of these should provide the same result
    myCombDat1$addDataSets(list(dataSet[[1]], dataSet[[2]], dataSet[[3]]))
    myCombDat2$addDataSets(c(dataSet[[1]], dataSet[[2]], dataSet[[3]]))

    expect_equal(myCombDat1$toDataFrame(), myCombDat2$toDataFrame())
  })

  # data transformations --------------------------

  test_that("data transformations don't work with arguments of wrong length", {
    myCombDat <- DataCombined$new()
    myCombDat$addSimulationResults(simResults)
    myCombDat$addDataSets(dataSet)

    expect_error(
      myCombDat$setDataTransformations(
        forNames = list(
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
        forNames = list(
          "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
          "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying"
        ),
        yOffsets = list(2, 5, 6)
      )
    )

    expect_error(
      myCombDat$setDataTransformations(
        forNames = list(
          "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
          "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying"
        ),
        xOffsets = list(2, 3, 6, 7),
        xScaleFactors = list(1.5, 2.4),
        yOffsets = c(4, 7, 8),
        yScaleFactors = c(1.1)
      )
    )
  })

  test_that("data transformations don't work with arguments of wrong type", {
    myCombDat <- DataCombined$new()
    myCombDat$addSimulationResults(simResults)
    myCombDat$addDataSets(dataSet)

    expect_error(
      myCombDat$setDataTransformations(
        forNames = list(
          "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
          "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying"
        ),
        xOffsets = list("0", "1")
      )
    )

    expect_error(myCombDat$setDataTransformations(
      forNames = 2,
      xOffsets = "2"
    ))

    expect_error(myCombDat$setDataTransformations(
      forNames = c(
        "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
        "Stevens_2012_placebo.Sita_proximal"
      ),
      xOffsets = c("2", 3),
      xScaleFactors = c("1.5", 2.4)
    ))
  })

  # useful across many tests
  names_ls <- list(
    "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
    "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
    "Stevens_2012_placebo.Sita_proximal",
    "Stevens_2012_placebo.Placebo_distal"
  )

  test_that("transformed values are equal to raw values times scale factor plus offsets - same transformations for each dataset", {
    myCombDat <- DataCombined$new()
    myCombDat$addSimulationResults(simResults)
    myCombDat$addDataSets(dataSet)

    # original data frame
    dfOriginal <- myCombDat$toDataFrame()
    dfOriginal <- dplyr::filter(dfOriginal, name %in% names_ls)

    myCombDat$setDataTransformations(
      forNames = names_ls,
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
  })


  test_that("transformed values are equal to raw values times scale factor plus offsets - different transformations for each dataset", {
    myCombDat <- DataCombined$new()
    myCombDat$addDataSets(dataSet)
    myCombDat$addSimulationResults(simResults)

    dfOriginal <- myCombDat$toDataFrame()
    dfOriginal <- dplyr::filter(dfOriginal, name %in% names_ls)

    myCombDat$setDataTransformations(
      forNames = list(
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
      myCombDat$dataTransformations,
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

    df <- myCombDat$toDataFrame()

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
  })

  test_that("each call to set transformations resets previous parameters for the same dataset", {
    myCombDat3 <- DataCombined$new()

    myCombDat3$addSimulationResults(simResults, quantitiesOrPaths = "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying")
    myCombDat3$setDataTransformations()
    df1 <- myCombDat3$dataTransformations
    dfDat1 <- myCombDat3$toDataFrame()

    myCombDat3$setDataTransformations(xOffsets = 3, yScaleFactors = 4)
    df2 <- myCombDat3$dataTransformations
    dfDat2 <- myCombDat3$toDataFrame()

    myCombDat3$setDataTransformations()
    df3 <- myCombDat3$dataTransformations
    dfDat3 <- myCombDat3$toDataFrame()

    # transformation values used
    expect_equal(df1, df3)
    expect_equal(df2$xOffsets, 3)
    expect_equal(df2$yScaleFactors, 4)

    # making sure transformations actually change values as expected
    expect_equal(dfDat1, dfDat3)
    expect_equal(dfDat1$xValues + 3, dfDat2$xValues)
    expect_equal(dfDat1$yValues * 4, dfDat2$yValues)
  })

  test_that("messy inputs (with special constants) for data transformations don't cause any problems", {
    myCombDat <- DataCombined$new()
    myCombDat$addDataSets(dataSet)
    myCombDat$addSimulationResults(simResults)

    myCombDat$setDataTransformations(
      forNames = names_ls,
      xOffsets = list(NULL, NA, NA_character_, NULL),
      yOffsets = c(NA, NA_real_, NA, NaN),
      xScaleFactors = c(NA, NA_integer_, NA, NA),
      yScaleFactors = list(NULL, NaN, NA, NULL)
    )

    expect_equal(
      myCombDat$dataTransformations,
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


  test_that("data grouping works as expected - multiple datasets", {
    myCombDat <- DataCombined$new()

    myCombDat$addSimulationResults(simResults)
    myCombDat$setGroups(
      names = list(
        "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
        "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
        "Organism|Lumen|Stomach|Metformin|Gastric retention"
      ),
      groups = list("distal", "proximal", "total")
    )

    myCombDat$addDataSets(dataSet)
    myCombDat$setGroups(
      names = list(
        "Stevens_2012_placebo.Placebo_total",
        "Stevens_2012_placebo.Sita_total",
        "Stevens_2012_placebo.Placebo_proximal",
        "Stevens_2012_placebo.Sita_proximal",
        "Stevens_2012_placebo.Placebo_distal",
        "Stevens_2012_placebo.Sita_dist"
      ),
      groups = list("total", "total", "proximal", "proximal", "distal", "distal")
    )

    # check mapping
    dfMap <- myCombDat$groupMap

    expect_equal(dim(dfMap), c(11L, 3L))

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
  })

  test_that("data grouping works as expected - single dataset", {
    myCombDat <- DataCombined$new()
    myCombDat$addDataSets(dataSet[[1]])
    myCombDat$setGroups(names = list("Stevens_2012_placebo.Placebo_total"), groups = list("x"))

    expect_equal(myCombDat$groupMap$group[[1]], "x")
    expect_equal(myCombDat$groupMap$name[[1]], "Stevens_2012_placebo.Placebo_total")
  })


  test_that("sequential update when first and second datasets have same names and same data - data frames and bindings should be identical", {
    myCombDat <- DataCombined$new()

    # first run

    myCombDat$addDataSets(dataSet)
    myCombDat$setGroups(
      names = list(
        "Stevens_2012_placebo.Placebo_total",
        "Stevens_2012_placebo.Sita_total",
        "Stevens_2012_placebo.Placebo_proximal",
        "Stevens_2012_placebo.Sita_proximal",
        "Stevens_2012_placebo.Placebo_distal",
        "Stevens_2012_placebo.Sita_dist"
      ),
      groups = list("total", "total", "proximal", "proximal", "distal", "distal")
    )

    myCombDat$addSimulationResults(simResults)
    myCombDat$setGroups(
      names = list(
        "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
        "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
        "Organism|Lumen|Stomach|Metformin|Gastric retention"
      ),
      groups = list("distal", "proximal", "total")
    )

    df1 <- myCombDat$toDataFrame()

    # second run but with different grouping
    myCombDat$addSimulationResults(simResults)
    myCombDat$setGroups(
      names = list(
        "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
        "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention"
      ),
      groups = list("Dapagliflozin - emptying", "Dapagliflozin - retention")
    )

    myCombDat$addDataSets(dataSet)
    myCombDat$setGroups(
      names = list(
        "Stevens_2012_placebo.Placebo_distal",
        "Stevens_2012_placebo.Sita_dist"
      ),
      groups = list("distal", "distal")
    )

    df2 <- myCombDat$toDataFrame()

    # should be the same number of rows and columns, and same raw data
    expect_equal(nrow(df2), nrow(df1))
    expect_equal(length(df2), length(df1))
    expect_equal(head(df1$yValues), head(df2$yValues))

    # names should also be the same since they are the same objects
    expect_equal(unique(df1$name), unique(df2$name))

    # but groupings should be different
    expect_equal(
      unique(df1$group),
      c(NA_character_, "total", "distal", "proximal")
    )

    expect_equal(
      unique(df2$group),
      c("Dapagliflozin - emptying", "Dapagliflozin - retention", NA_character_, "distal")
    )

    # extract group map into a data frame
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

  test_that("sequential update when first and second datasets have same names but different data - second should replace first", {
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
    expect_equal(dim(df1), c(77L, 17L))
    expect_equal(dim(df2), c(76L, 17L))
    expect_equal(dim(df1Filter), c(25L, 17L))
    expect_equal(dim(df2Filter), c(24L, 17L))

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

  # `Population` objects -----------------------------

  test_that("data frame is as expected when `Population` objects are used", {


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

    expect_equal(dim(df), c(1964L, 12L))

    expect_equal(min(df$IndividualId), 1)
    expect_equal(max(df$IndividualId), 44)
    expect_equal(unique(df$yUnit), "µmol/l")
    expect_equal(unique(df$yDimension), "Concentration (molar)")
    expect_equal(unique(df$xUnit), "min")
  })

  # `DataSet` with metadata -----------------------------

  myDataSet <- dataSet$Stevens_2012_placebo.Placebo_total
  myDataSet$addMetaData("Organ", "Liver")
  myDataSet$addMetaData("Compartment", "Intracellular")
  myDataSet$addMetaData("Species", "Human")

  test_that("data frame dimensions are as expected when `DataSet` with metadata is provided", {
    myCombDat <- DataCombined$new()
    myCombDat$addDataSets(myDataSet)
    df <- myCombDat$toDataFrame()

    expect_equal(dim(df), c(12L, 20L))
  })

  test_that("data frame column names are as expected when `DataSet` with metadata is provided", {
    myCombDat <- DataCombined$new()
    myCombDat$addDataSets(myDataSet)
    df <- myCombDat$toDataFrame()

    expect_equal(
      names(df),
      c(
        "name", "group", "dataType", "xValues", "xUnit", "xDimension",
        "yValues", "yUnit", "yDimension", "yErrorValues", "yErrorType",
        "yErrorUnit", "molWeight", "lloq", "Source", "Sheet", "Group Id",
        "Organ", "Compartment", "Species"
      )
    )
  })

  test_that("data frame metadata column entries are as expected when `DataSet` with metadata is provided", {
    myCombDat <- DataCombined$new()
    myCombDat$addDataSets(myDataSet)
    df <- myCombDat$toDataFrame()

    expect_equal(unique(df$Organ), "Liver")
    expect_equal(unique(df$Compartment), "Intracellular")
    expect_equal(unique(df$Species), "Human")
  })
}
