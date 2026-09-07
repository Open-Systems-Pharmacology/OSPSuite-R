# Rebuilds inst/extdata/Aciclovir.pkml from the PK-Sim project snapshot in
# data-raw/Aciclovir_snapshot.json. Run from the repository root:
#
#   Rscript data-raw/regenerate-aciclovir-pkml.R
#
# IMPORTANT: a simulation returned by loadSimulationsFromSnapshot() carries no
# embedded PK-Sim snapshot, so the PKML written here has EMPTY <Snapshot>
# elements and the example file loses the snapshot it has today.
# data-raw/Aciclovir_snapshot.json is therefore the source of truth and is kept
# under version control. Read data-raw/README.md before you run this, it
# describes the alternative route through the PK-Sim user interface that keeps
# the embedded snapshot in the file.

devtools::load_all(".", quiet = TRUE)

pkmlPath <- file.path("inst", "extdata", "Aciclovir.pkml")
snapshotFile <- file.path("data-raw", "Aciclovir_snapshot.json")

# 1. Get the PK-Sim project snapshot. Normally it is already in data-raw/. The
# extraction below is the one-time bootstrap for a PKML that still carries the
# snapshot inside itself: three <Snapshot> elements are present, and the project
# one is identified by its ApplicationName entry.
if (!file.exists(snapshotFile)) {
  doc <- xml2::read_xml(pkmlPath)
  jsons <- vapply(
    xml2::xml_find_all(doc, "//Snapshot"),
    function(node) rawToChar(base64enc::base64decode(xml2::xml_text(node))),
    FUN.VALUE = character(1)
  )
  isProject <- grepl('"ApplicationName": "PK-Sim"', jsons, fixed = TRUE)
  if (sum(isProject) != 1) {
    stop(
      "Expected exactly one PK-Sim project snapshot in ",
      pkmlPath,
      ", found ",
      sum(isProject),
      ". Restore data-raw/Aciclovir_snapshot.json from git instead."
    )
  }
  writeLines(jsons[isProject][[1]], snapshotFile, useBytes = TRUE)
  message("Extracted snapshot from the PKML: ", snapshotFile)
} else {
  message("Using snapshot: ", snapshotFile)
}

# 2. Load the snapshot. PK-Sim rebuilds the simulation with the current libraries.
simulations <- loadSimulationsFromSnapshot(snapshotFile)
if (length(simulations) != 1) {
  stop(
    "Expected one simulation in the snapshot, found ",
    length(simulations),
    ": ",
    paste(names(simulations), collapse = ", ")
  )
}

# 3. Export the single simulation to the example file.
saveSimulation(simulations[[1]], pkmlPath)
message("Wrote: ", pkmlPath)

# 4. Report what came out, so the values can be compared with the README.
check <- loadSimulation(pkmlPath)
message("Simulation name: ", check$name)
message(
  "Outputs: ",
  paste(
    vapply(
      check$outputSelections$allOutputs,
      function(output) output$path,
      FUN.VALUE = character(1)
    ),
    collapse = " ; "
  )
)
