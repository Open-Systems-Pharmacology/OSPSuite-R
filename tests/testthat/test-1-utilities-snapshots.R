test_that("Run simulation from snapshot works", {
  path <- system.file("extdata", "test_snapshot.json", package = "ospsuite")

  temp_dir <- withr::local_tempdir()

  runSimulationsFromSnapshot(
    path,
    output = temp_dir,
    exportCSV = TRUE,
    exportPKML = TRUE,
    exportJSON = TRUE,
    exportXML = TRUE
  )
  expect_length(list.files(temp_dir, pattern = ".csv"), 3)
  expect_length(list.files(temp_dir, pattern = ".pkml"), 2)
  expect_length(list.files(temp_dir, pattern = ".json"), 2)
  expect_length(list.files(temp_dir, pattern = ".xml"), 2)
})

test_that("RunForAllOutputs argument works", {
  path <- system.file("extdata", "test_snapshot.json", package = "ospsuite")

  temp_dir1 <- withr::local_tempdir()

  runSimulationsFromSnapshot(
    path,
    output = temp_dir1,
    RunForAllOutputs = FALSE,
    exportCSV = TRUE
  )

  temp_dir2 <- withr::local_tempdir()

  runSimulationsFromSnapshot(
    path,
    output = temp_dir2,
    RunForAllOutputs = TRUE,
    exportCSV = TRUE
  )

  for (file_name in list.files(temp_dir1, pattern = "Results.csv")) {
    # test if the number of columns are differents in files in temp_dir1 and temp_dir2
    expect_true(
      ncol(read.csv(file.path(temp_dir1, file_name))) <
        ncol(read.csv(file.path(temp_dir2, file_name)))
    )
  }
})

test_that("runSimulationsFromSnapshot arguments are checked", {
  path <- system.file("extdata", "test_snapshot.json", package = "ospsuite")

  temp_dir <- withr::local_tempdir()

  expect_error(runSimulationsFromSnapshot(path, exportCSV = "path/to/my.csv"))
  expect_error(runSimulationsFromSnapshot(path, output = 1))

  # provide wrong input/output paths
  expect_error(runSimulationsFromSnapshot(
    "wrong_file.json",
    "wrong/path",
    output = "wrong/output/path"
  ))
})

test_that("loadProjectFromSnapshot converts a snapshot to a project", {
  path <- system.file("extdata", "test_snapshot.json", package = "ospsuite")
  temp_dir <- withr::local_tempdir()
  loadProjectFromSnapshot(path, output = temp_dir)

  expect_length(list.files(temp_dir, pattern = ".pksim5"), 1)
})

test_that("exportProjectToSnapshot converts a project to a snapshot", {
  path <- getTestDataFilePath("test_project.pksim5")
  temp_dir <- withr::local_tempdir()
  exportProjectToSnapshot(path, output = temp_dir)

  expect_length(list.files(temp_dir, pattern = ".json"), 1)
})

test_that("loadProjectFromSnapshot runSimulations argument is supported", {
  path <- system.file("extdata", "test_snapshot.json", package = "ospsuite")
  temp_dir <- withr::local_tempdir()
  expect_no_error({
    loadProjectFromSnapshot(path, output = temp_dir, runSimulations = TRUE)
    loadProjectFromSnapshot(path, output = temp_dir, runSimulations = FALSE)
  })
})

test_that("convertSnapshot is deprecated but still delegates", {
  # setup.R silences lifecycle warnings globally; force them on so the
  # deprecation warning is emitted and can be captured here.
  withr::local_options(lifecycle_verbosity = "warning")

  path <- system.file("extdata", "test_snapshot.json", package = "ospsuite")
  temp_dir <- withr::local_tempdir()

  expect_warning(
    convertSnapshot(path, output = temp_dir, format = "project"),
    regexp = "deprecated"
  )
  expect_length(list.files(temp_dir, pattern = ".pksim5"), 1)

  path <- getTestDataFilePath("test_project.pksim5")
  temp_dir <- withr::local_tempdir()
  expect_warning(
    convertSnapshot(path, output = temp_dir, format = "snapshot"),
    regexp = "deprecated"
  )
  expect_length(list.files(temp_dir, pattern = ".json"), 1)
})

test_that("gather files  handles one file path", {
  # create a temporary file
  temp_file <- withr::local_tempfile(fileext = ".json", lines = "content")
  # .gatherFiles should copy the file to a new temporary directory
  new_temp_dir <- .gatherFiles(temp_file)
  expect_true(length(list.files(new_temp_dir, pattern = ".json")) == 1)
})

test_that("gather files  handles several file paths", {
  # create two separate temp directory
  temp_dir1 <- withr::local_tempdir()
  temp_dir2 <- withr::local_tempdir()

  # create two files in temp_dir
  files <- file.path(c(temp_dir1, temp_dir2), c("file1.json", "file2.json"))
  file.create(files)

  # .gatherFiles should copy the file to a new temporary directory
  new_temp_dir <- .gatherFiles(files)
  expect_true(length(list.files(new_temp_dir, pattern = ".json")) == 2)
})

test_that("gather files handles a directory with several files", {
  # create a temp directory
  temp_dir <- withr::local_tempdir()

  # create two files in temp_dir
  files <- file.path(temp_dir, c("file1.json", "file2.json"))
  file.create(files)

  # .gatherFiles should copy the file to a new temporary directory
  new_temp_dir <- .gatherFiles(temp_dir)
  expect_true(length(list.files(new_temp_dir, pattern = ".json")) == 2)
})

test_that("gather files handles files and directories", {
  # create a temp directory
  temp_dir <- withr::local_tempdir()

  # create two files in a subdir and one file in temp_dir
  sub_dir <- withr::local_tempdir(tmpdir = temp_dir)
  dir_files <- file.path(sub_dir, c("file1.json", "file2.json"))
  file <- file.path(temp_dir, "file.json")
  files <- c(dir_files, file)
  file.create(files)

  new_temp_dir <- .gatherFiles(file, sub_dir)
  expect_true(length(list.files(new_temp_dir, pattern = ".json")) == 3)
})

###### Load simulations from snapshot
# Tests for `loadSimulationsFromSnapshot()`, the R wrapper around the SnapshotTask
# `LoadSimulationsFromSnapshot` API. It returns the simulations stored in a
# snapshot file as `Simulation` objects, or - when simulation names are supplied -
# only the matching simulations (case-sensitive match).

snapshotFile <- system.file(
  "extdata",
  "test_snapshot.json",
  package = "ospsuite"
)

test_that("loadSimulationsFromSnapshot returns every simulation in the snapshot", {
  simulations <- loadSimulationsFromSnapshot(snapshotFile)

  expect_length(simulations, 2)
  expect_true(isOfType(simulations, Simulation))
})

test_that("loadSimulationsFromSnapshot returns a list named by simulation name", {
  simulations <- loadSimulationsFromSnapshot(snapshotFile)

  expect_named(simulations)
  expect_equal(
    names(simulations),
    vapply(simulations, function(sim) sim$name, character(1), USE.NAMES = FALSE)
  )
})

test_that("loadSimulationsFromSnapshot returns only simulations whose name matches", {
  simName <- "Simulation - IV + Weibull - Default tolerance"

  simulations <- loadSimulationsFromSnapshot(
    snapshotFile,
    simulationNames = simName
  )

  expect_length(simulations, 1)
  expect_equal(simulations[[1]]$name, simName)
})

test_that("loadSimulationsFromSnapshot errors for a non-existing name", {
  expect_error(
    loadSimulationsFromSnapshot(
      snapshotFile,
      simulationNames = "ThisSimulationDoesNotExist"
    ),
    regexp = "not found in the snapshot"
  )
})

test_that("loadSimulationsFromSnapshot errors when only some names are found", {
  existingName <- "Simulation - IV + Weibull - Default tolerance"
  expect_error(
    loadSimulationsFromSnapshot(
      snapshotFile,
      simulationNames = c(existingName, "ThisSimulationDoesNotExist")
    ),
    regexp = "ThisSimulationDoesNotExist"
  )
})

test_that("loadSimulationsFromSnapshot returns NULL for missing names when ignoreIfNotFound = TRUE", {
  existingName <- "Simulation - IV + Weibull - Default tolerance"
  requested <- c(existingName, "ThisSimulationDoesNotExist")

  simulations <- loadSimulationsFromSnapshot(
    snapshotFile,
    simulationNames = requested,
    ignoreIfNotFound = TRUE
  )

  expect_length(simulations, 2)
  expect_named(simulations, requested)
  expect_true(isOfType(simulations[[1]], Simulation))
  expect_null(simulations[[2]])
})

test_that("a simulation loaded from a snapshot can be run", {
  simulation <- loadSimulationsFromSnapshot(snapshotFile)[[1]]

  results <- runSimulations(simulation)[[1]]
  expect_true(isOfType(results, "SimulationResults"))
})

test_that("loadSimulationsFromSnapshot validates its arguments", {
  expect_error(
    loadSimulationsFromSnapshot(snapshotFile, simulationNames = 1),
    regexp = "expected <character>"
  )
  expect_error(
    loadSimulationsFromSnapshot("does_not_exist.json"),
    regexp = "does not exist"
  )
})

# End-to-end "seam" test (OSPSuite-R#1981): a population snapshot ->
# loadSimulationsFromSnapshot -> runSimulations on a mixed list ->
# population results (Count > 1) with aging applied. This exercises the full
# cross-repo chain (PK-Sim snapshot converter + OSPSuite.Core RunSimulations).
test_that("a population snapshot loads and runs through runSimulations (cross-repo seam)", {
  snapshotFile <- system.file(
    "extdata",
    "ind_and_pop_snapshot.json",
    package = "ospsuite"
  )

  simulations <- loadSimulationsFromSnapshot(snapshotFile)
  expect_length(simulations, 2)

  # population vs individual is distinguished by the IsPopulation flag on the .NET object
  populationSim <- Filter(function(s) s$get("IsPopulation"), simulations)[[1]]
  individualSim <- Filter(function(s) !s$get("IsPopulation"), simulations)[[1]]

  # the snapshot converter carried the population and aging data onto the underlying .NET object,
  # readable via rSharp $get() (there is no R6 accessor)
  expect_equal(populationSim$get("IndividualValuesCache")$get("Count"), 6)
  expect_false(is.null(populationSim$get("AgingData")))
  expect_true(is.null(individualSim$get("IndividualValuesCache")))

  # the snapshot does not define output selections, so add an output to each simulation
  for (simulation in simulations) {
    addOutputs(
      getAllQuantitiesMatching(
        "Organism|PeripheralVenousBlood|*|Plasma (Peripheral Venous Blood)",
        simulation
      ),
      simulation
    )
  }

  results <- runSimulations(simulations)
  expect_length(results, 2)

  # loadSimulationsFromSnapshot returns a list named by simulation name, which
  # runSimulations reuses for the result list
  populationResults <- results[[populationSim$name]]
  individualResults <- results[[individualSim$name]]

  expect_true(isOfType(populationResults, "SimulationResults"))
  # Count > 1 proves it ran as a population (with aging applied)
  expect_equal(populationResults$count, 6)
  expect_equal(individualResults$count, 1)
})
