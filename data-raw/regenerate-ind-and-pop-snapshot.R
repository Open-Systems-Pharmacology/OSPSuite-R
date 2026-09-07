# Rebuilds inst/extdata/ind_and_pop_snapshot.json by converting it to a PK-Sim
# project with the current libraries and back to a snapshot. Run from the
# repository root:
#
#   Rscript data-raw/regenerate-ind-and-pop-snapshot.R
#
# See data-raw/README.md for the background and for what to check afterwards.

devtools::load_all(".", quiet = TRUE)

snapshotPath <- file.path("inst", "extdata", "ind_and_pop_snapshot.json")

workDir <- tempfile("ind_and_pop_")
projectDir <- file.path(workDir, "project")
snapshotDir <- file.path(workDir, "snapshot")
dir.create(projectDir, recursive = TRUE)
dir.create(snapshotDir, recursive = TRUE)

# 1. Snapshot to project. runSimulations = TRUE is required: the committed
# snapshot has "HasResults": true, which only a project with results carries back
# into the export.
loadProjectFromSnapshot(snapshotPath, output = projectDir, runSimulations = TRUE)

projectFiles <- list.files(projectDir, full.names = TRUE)
message("Project: ", paste(basename(projectFiles), collapse = ", "))

# 2. Project back to snapshot. The output keeps the base name of the project file.
exportProjectToSnapshot(projectFiles, output = snapshotDir)

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
invisible(file.copy(exported, snapshotPath, overwrite = TRUE))
message("Wrote: ", snapshotPath)

# 4. Report what came out: two simulations, one of them a population of 6 with
# aging data (as asserted in tests/testthat/test-1-utilities-snapshots.R).
simulations <- loadSimulationsFromSnapshot(snapshotPath)
message("Simulations: ", paste(names(simulations), collapse = ", "))
for (simulation in simulations) {
  cache <- simulation$get("IndividualValuesCache")
  message(
    "  ",
    simulation$name,
    ": isPopulation = ",
    simulation$get("IsPopulation"),
    ", individuals = ",
    if (is.null(cache)) 1 else cache$get("Count"),
    ", aging data = ",
    !is.null(simulation$get("AgingData"))
  )
}
