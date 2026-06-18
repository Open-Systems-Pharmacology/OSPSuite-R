test_that("Run simulation from snapshot works", {
  path <- getTestDataFilePath("test_snapshot.json")

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
  path <- getTestDataFilePath("test_snapshot.json")

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
  path <- getTestDataFilePath("test_snapshot.json")

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
  path <- getTestDataFilePath("test_snapshot.json")
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
  path <- getTestDataFilePath("test_snapshot.json")
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

  path <- getTestDataFilePath("test_snapshot.json")
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
