#' Run Simulations From Snapshot Files
#'
#' @param ... character strings, path to snapshot files or a directory containing snapshot files
#' @param output character string, path to the output directory where to write simulation results
#' @param RunForAllOutputs logical, whether to run the simulation for all outputs or only OutputSelections (default = FALSE)
#' @param exportCSV logical, whether to export the results as csv (default = TRUE)
#' @param exportPKML logical, whether to export the results as pkml (default = FALSE)
#' @param exportJSON logical, whether to export simulation results as json (default = FALSE)
#' @param exportXML logical, whether to export the results as xml (default = FALSE)
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' runSimulationsFromSnapshot("path/to/my_snapshot.json", csv = TRUE, pkml = TRUE)
#' }
runSimulationsFromSnapshot <- function(
  ...,
  output = ".",
  RunForAllOutputs = FALSE,
  exportCSV = TRUE,
  exportPKML = FALSE,
  exportJSON = FALSE,
  exportXML = FALSE
) {
  if (Sys.info()[["sysname"]] == "Darwin") {
    cli::cli_abort(
      "runSimulationsFromSnapshot is currently not supported on macOS."
    )
  }

  ospsuite.utils::validateIsLogical(
    object = c(exportCSV, exportPKML, exportXML, RunForAllOutputs)
  )
  ospsuite.utils::validateIsCharacter(object = c(..., output))

  paths_exist <- file.exists(c(..., output))
  if (!all(paths_exist)) {
    missing_paths <- c(..., output)[!paths_exist]
    cli::cli_abort(
      message = c(
        "x" = "Some of the paths provided do not exist: {.file {missing_paths}}"
      )
    )
  }

  initPKSim()

  temp_dir <- .gatherFiles(c(...))

  JsonRunOptions <- rSharp::newObjectFromName(
    "PKSim.CLI.Core.RunOptions.JsonRunOptions"
  )
  JsonRunOptions$set("InputFolder", temp_dir)
  JsonRunOptions$set("OutputFolder", normalizePath(output))
  JsonRunOptions$set("RunForAllOutputs", RunForAllOutputs)

  if (isTRUE(exportJSON)) {
    exportJSON <- 1L
  } else {
    exportJSON <- 0L
  }
  if (isTRUE(exportCSV)) {
    exportCSV <- 2L
  } else {
    exportCSV <- 0L
  }
  if (isTRUE(exportXML)) {
    exportXML <- 4L
  } else {
    exportXML <- 0L
  }
  if (isTRUE(exportPKML)) {
    exportPKML <- 8L
  } else {
    exportPKML <- 0L
  }

  ExportMode <- exportJSON + exportCSV + exportXML + exportPKML
  # 1: json
  # 2: csv
  # 3: json + csv
  # 4: xml
  # 5: xml + json
  # 6: xml + csv
  # 7: json + csv + xml
  # 8: pkml
  # 9: pkml + json
  # 10: pkml + csv
  # 11: pkml + json + csv
  # 12: pkml + xml
  # 13: pkml + xml + json
  # 14: pkml + xml + csv
  # 15: all

  JsonRunOptions$set("ExportMode", ExportMode)

  cli::cli_process_start(
    msg = "Running simulations from {length(list.files(temp_dir))} snapshot{?s}",
    msg_done = "Simulations completed",
    msg_failed = "An error occured while running simulation"
  )

  tryCatch(
    {
      invisible(rSharp::callStatic("PKSim.R.Api", "RunJson", JsonRunOptions))
    },
    error = function(e) {
      message <- stringr::str_extract(as.character(e), "(?<=Message: )[^\\n]*")

      cli::cli_abort(message = message, call = rlang::caller_env(n = 4))
    }
  )
}

#' Load a project from a snapshot
#'
#' @description
#' Converts one or more snapshot files (`.json`) into project files and writes
#' them to an output directory.
#'
#' Only PK-Sim projects (`.pksim5`) are supported for now. Support for MoBi
#' projects is planned.
#'
#' @param ... character strings, path to snapshot files (`.json`) or a directory
#'   containing snapshot files to convert.
#' @param output character string, path to the output directory where to write
#'   the converted project files.
#' @param runSimulations logical, whether to run the simulations during
#'   conversion (default = `FALSE`).
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' loadProjectFromSnapshot("path/to/snapshot.json", output = "path/to/output")
#' }
loadProjectFromSnapshot <- function(..., output = ".", runSimulations = FALSE) {
  temp_dir <- .gatherFiles(c(...))
  nfiles <- length(list.files(temp_dir, pattern = ".json"))

  .runSnapshotConversion(
    inputFolder = temp_dir,
    output = output,
    exportMode = 0L,
    runSimulations = runSimulations,
    nfiles = nfiles,
    targetFormat = "project"
  )
}

#' Export a project to a snapshot
#'
#' @description
#' Converts one or more project files into snapshot files (`.json`) and writes
#' them to an output directory.
#'
#' Only PK-Sim projects (`.pksim5`) are supported for now. Support for MoBi
#' projects is planned.
#'
#' @param ... character strings, path to project files (`.pksim5`) or a directory
#'   containing project files to convert.
#' @param output character string, path to the output directory where to write
#'   the converted snapshot files.
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' exportProjectToSnapshot("path/to/project.pksim5", output = "path/to/output")
#' }
exportProjectToSnapshot <- function(..., output = ".") {
  temp_dir <- .gatherFiles(c(...))
  nfiles <- length(list.files(temp_dir, pattern = ".pksim5"))

  .runSnapshotConversion(
    inputFolder = temp_dir,
    output = output,
    exportMode = 1L,
    runSimulations = FALSE,
    nfiles = nfiles,
    targetFormat = "snapshot"
  )
}

#' Convert between snapshot and project formats
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `convertSnapshot()` is deprecated and will be removed in a future release.
#' Use [loadProjectFromSnapshot()] to convert a snapshot to a project, and
#' [exportProjectToSnapshot()] to convert a project to a snapshot.
#'
#' @param ... character strings, path to files or a directory containing files to convert
#' @param format, character string, target format either "snapshot" or "project".
#' @param output character string, path to the output directory where to write the converted files
#' @param runSimulations logical, whether to run simulations during conversion (default = FALSE).
#' Only when converting from snapshot to project.
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' convertSnapshot("path/to/snapshot.json", format = "project")
#' convertSnapshot("path/to/project.pksim5", format = "snapshot")
#' }
convertSnapshot <- function(..., format, output = ".", runSimulations = FALSE) {
  rlang::arg_match(arg = format, values = c("snapshot", "project"))

  if (format == "project") {
    lifecycle::deprecate_warn(
      when = "12.4.4",
      what = "convertSnapshot()",
      with = "loadProjectFromSnapshot()"
    )
    loadProjectFromSnapshot(
      ...,
      output = output,
      runSimulations = runSimulations
    )
  } else {
    lifecycle::deprecate_warn(
      when = "12.4.4",
      what = "convertSnapshot()",
      with = "exportProjectToSnapshot()"
    )
    exportProjectToSnapshot(..., output = output)
  }
}

#' Run a snapshot/project conversion via the PK-Sim core
#'
#' @param inputFolder character string, directory holding the files to convert.
#' @param output character string, output directory for the converted files.
#' @param exportMode integer, `0L` for snapshot -> project, `1L` for project -> snapshot.
#' @param runSimulations logical, whether to run simulations during conversion.
#' @param nfiles integer, number of input files (used for the progress message).
#' @param targetFormat character string, target format used in the progress message.
#'
#' @return NULL
#' @keywords internal
#' @noRd
.runSnapshotConversion <- function(
  inputFolder,
  output,
  exportMode,
  runSimulations,
  nfiles,
  targetFormat
) {
  # Attribute conversion errors to the public function that called this helper
  # (loadProjectFromSnapshot/exportProjectToSnapshot). Forced here, not in the
  # error handler, so it resolves to this frame's caller rather than the handler.
  call <- rlang::caller_env()

  initPKSim()

  SnapshotRunOptions <- rSharp::newObjectFromName(
    "PKSim.CLI.Core.RunOptions.SnapshotRunOptions"
  )
  SnapshotRunOptions$set(name = "InputFolder", value = inputFolder)
  SnapshotRunOptions$set(name = "OutputFolder", value = normalizePath(output))
  SnapshotRunOptions$set(
    name = "RunSimulations",
    value = isTRUE(runSimulations)
  )
  SnapshotRunOptions$set("ExportMode", exportMode)

  cli::cli_process_start(
    msg = "Converting {nfiles} file{?s} to {targetFormat} format",
    msg_done = "Conversion completed",
    msg_failed = "An error occured while converting files"
  )

  tryCatch(
    {
      invisible(rSharp::callStatic(
        "PKSim.R.Api",
        "RunSnapshot",
        SnapshotRunOptions
      ))
    },
    error = function(e) {
      message <- stringr::str_extract(as.character(e), "(?<=Message: )[^\\n]*")

      if (is.na(message)) {
        message <- e
      }

      cli::cli_abort(message = message, call = call)
    }
  )
}


#' Gather files and files from folders to one location
#'
#' @param ... character strings of file paths or folder paths
#'
#' @return A temporary directory with all files copied to it
.gatherFiles <- function(...) {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  for (element in c(...)) {
    # if the element is a folder, list all files in it and copy them to the temp directory
    if (dir.exists(element)) {
      files <- list.files(element, full.names = TRUE, recursive = TRUE)
      for (file in files) {
        file.copy(from = file, to = temp_dir)
      }
      next
    } else if (file.exists(element)) {
      # if the element is a file, copy it to the temp directory
      file.copy(from = element, to = temp_dir)
      next
    }
  }
  return(temp_dir)
}
