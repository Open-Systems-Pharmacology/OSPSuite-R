# Rebuilds inst/extdata/test_snapshot.json by converting it to a PK-Sim project
# with the current libraries and back to a snapshot. Run from the repository
# root:
#
#   Rscript data-raw/regenerate-test-snapshot.R
#
# See data-raw/README.md.

devtools::load_all(".", quiet = TRUE)

snapshotPath <- file.path("inst", "extdata", "test_snapshot.json")

workDir <- tempfile("test_snapshot_")
projectDir <- file.path(workDir, "project")
snapshotDir <- file.path(workDir, "snapshot")
dir.create(projectDir, recursive = TRUE)
dir.create(snapshotDir, recursive = TRUE)

# 1. Snapshot to project. The simulations in this snapshot have
# "HasResults": false, so they are not run.
snapshotToProject(
  snapshotPath,
  output = projectDir,
  runSimulations = FALSE
)

projectFiles <- list.files(projectDir, full.names = TRUE)
message("Project: ", paste(basename(projectFiles), collapse = ", "))

# 2. Project back to snapshot. The output keeps the base name of the project file.
projectToSnapshot(projectFiles, output = snapshotDir)

exported <- file.path(snapshotDir, basename(snapshotPath))
if (!file.exists(exported)) {
  stop(
    "Expected the exported snapshot at ",
    exported,
    ", found: ",
    paste(list.files(snapshotDir), collapse = ", ")
  )
}

# 3. Replace the example file.
if (!file.copy(exported, snapshotPath, overwrite = TRUE)) {
  stop("Could not copy ", exported, " to ", snapshotPath)
}
message("Wrote: ", snapshotPath)

# 4. Report what came out.
simulations <- loadSimulationsFromSnapshot(snapshotPath)
message("Simulations: ", paste(names(simulations), collapse = ", "))
message(
  "Snapshot version: ",
  sub(
    '.*"Version": ([0-9]+).*',
    "\\1",
    grep('"Version"', readLines(snapshotPath, warn = FALSE), value = TRUE)[[1]]
  )
)
