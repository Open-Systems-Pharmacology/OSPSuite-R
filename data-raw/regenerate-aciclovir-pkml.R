# Rebuilds inst/extdata/Aciclovir.pkml from the PK-Sim project snapshot that is
# embedded in the PKML itself. Run from the repository root:
#
#   Rscript data-raw/regenerate-aciclovir-pkml.R
#
# The extracted snapshot is written to data-raw/Aciclovir_snapshot.json and is
# kept under version control, because a simulation returned by
# loadSimulationsFromSnapshot() carries no embedded snapshot of its own: the
# PKML this script writes has empty <Snapshot> elements, so the next run has
# nothing left to extract and falls back to the committed JSON. See
# data-raw/README.md.

devtools::load_all(".", quiet = TRUE)

pkmlPath <- file.path("inst", "extdata", "Aciclovir.pkml")
snapshotFile <- file.path("data-raw", "Aciclovir_snapshot.json")

# 1. Extract the PK-Sim project snapshot from the PKML. Three <Snapshot>
# elements are present, one for the project, one for the individual and one for
# the expression profile; the project one is identified by its ApplicationName
# entry. A PKML written by this script has all three empty, in which case the
# committed JSON is used instead.
doc <- xml2::read_xml(pkmlPath)
jsons <- vapply(
  xml2::xml_find_all(doc, "//Snapshot"),
  function(node) rawToChar(base64enc::base64decode(xml2::xml_text(node))),
  FUN.VALUE = character(1)
)
isProject <- grepl('"ApplicationName": "PK-Sim"', jsons, fixed = TRUE)

if (sum(isProject) == 1) {
  writeLines(jsons[isProject][[1]], snapshotFile, useBytes = TRUE)
  message("Extracted snapshot from the PKML: ", snapshotFile)
} else if (file.exists(snapshotFile)) {
  message(
    "The PKML carries no project snapshot (",
    sum(isProject),
    " found). Using the committed one: ",
    snapshotFile
  )
} else {
  stop(
    "No project snapshot in ",
    pkmlPath,
    " and none at ",
    snapshotFile,
    ". Restore data-raw/Aciclovir_snapshot.json from git."
  )
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
