#' Run Simulations From Snapshot Files
#'
#' @param ... character strings, path to snapshot files or a directory containing snapshot files
#' @param output character string, path to the output directory where to write simulation results
#' @param RunForAllOutputs logical, whether to run the simulation for all outputs or only OutputSelections (default = FALSE)
#' @param exportCSV logical, whether to export the results as csv (default = TRUE)
#' @param exportPKML logical, whether to export the simulations as pkml (default = FALSE)
#' @param exportJSON logical, whether to export simulation results as json (default = FALSE)
#' @param exportXML logical, whether to export the simulations as xml (default = FALSE)
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
      invisible(rSharp::callStatic(
        "PKSim.R.Api, PKSim.R",
        "RunJson",
        JsonRunOptions
      ))
    },
    error = function(e) {
      message <- stringr::str_extract(as.character(e), "(?<=Message: )[^\\n]*")

      cli::cli_abort(message = message, call = rlang::caller_env(n = 4))
    }
  )
}

#' Validate inputs of the snapshot conversion functions
#'
#' @param inputs character vector, paths to the files/directories to convert.
#' @param output character string, the output directory.
#' @param runSimulations logical (or `NULL` to skip the check).
#'
#' @return NULL, called for its side effect of aborting on invalid input.
#' @keywords internal
#' @noRd
.validateSnapshotConversionInputs <- function(
  inputs,
  output,
  runSimulations = NULL
) {
  ospsuite.utils::validateIsCharacter(object = c(inputs, output))

  if (length(inputs) == 0L) {
    cli::cli_abort(
      message = c("x" = "Please provide at least one input path.")
    )
  }

  if (!is.null(runSimulations)) {
    ospsuite.utils::validateIsLogical(object = runSimulations)
  }

  missingInputs <- inputs[!file.exists(inputs)]
  if (length(missingInputs) > 0L) {
    cli::cli_abort(
      message = c(
        "x" = "Some of the input paths provided do not exist: {.file {missingInputs}}"
      )
    )
  }

  if (!dir.exists(output)) {
    cli::cli_abort(
      message = c(
        "x" = "The output directory does not exist: {.file {output}}"
      )
    )
  }

  invisible(NULL)
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
  inputs <- c(...)
  .validateSnapshotConversionInputs(
    inputs = inputs,
    output = output,
    runSimulations = runSimulations
  )

  temp_dir <- .gatherFiles(inputs)
  nfiles <- length(list.files(temp_dir, pattern = "\\.json$"))

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
  inputs <- c(...)
  .validateSnapshotConversionInputs(inputs = inputs, output = output)

  temp_dir <- .gatherFiles(inputs)
  nfiles <- length(list.files(temp_dir, pattern = "\\.pksim5$"))

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
    "OSPSuite.CLI.Core.RunOptions.SnapshotRunOptions"
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
        "PKSim.R.Api, PKSim.R",
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


#' Load simulations from a snapshot file
#'
#' @description
#' Loads the simulations stored in a snapshot file and returns them as a list of
#' [Simulation] objects. By default every simulation in the snapshot is loaded.
#' When `simulationNames` are supplied, only the simulations whose name matches
#' (case-sensitive) are returned. By default, an error is thrown if any of the
#' requested names is not present in the snapshot. Set `ignoreIfNotFound = TRUE`
#' to instead return `NULL` for the names that were not found.
#'
#' @param snapshotFile Character string, path to the snapshot file (`.json`).
#' @param simulationNames Optional character vector of simulation names to load.
#'   If `NULL` (default), all simulations in the snapshot are loaded.
#' @param ignoreIfNotFound Logical. If `FALSE` (default), an error is thrown when
#'   any of the requested `simulationNames` is not found in the snapshot. If
#'   `TRUE`, missing names are returned as `NULL` entries instead. Has no effect
#'   when `simulationNames` is `NULL`.
#'
#' @return A named list of [Simulation] objects, with names being the simulation
#'   names. When `simulationNames` is supplied, the returned list keeps the order
#'   of the requested names; with `ignoreIfNotFound = TRUE`, entries for names
#'   that were not found are `NULL`.
#'
#' @export
#'
#' @examples
#' snapshotPath <- system.file("extdata", "test_snapshot.json", package = "ospsuite")
#'
#' # Load every simulation from a snapshot
#' simulations <- loadSimulationsFromSnapshot(snapshotPath)
#'
#' # Load only a specific simulation by name
#' firstName <- simulations[[1]]$name
#' oneSimulation <- loadSimulationsFromSnapshot(
#'   snapshotPath,
#'   simulationNames = firstName
#' )
loadSimulationsFromSnapshot <- function(
  snapshotFile,
  simulationNames = NULL,
  ignoreIfNotFound = FALSE
) {
  validateIsString(snapshotFile)
  validateIsCharacter(simulationNames, nullAllowed = TRUE)
  validateIsLogical(ignoreIfNotFound)

  if (!file.exists(snapshotFile)) {
    cli::cli_abort(
      message = c(
        "x" = "The snapshot file provided does not exist: {.file {snapshotFile}}"
      )
    )
  }

  initPKSim()

  snapshotTask <- rSharp::callStatic("PKSim.R.Api, PKSim.R", "GetSnapshotTask")

  # `LoadSimulationsFromSnapshot(string file, params string[] names)` expects the
  # names spread as individual positional arguments so each is marshalled as an
  # element of the `params` array.
  args <- c(
    list("LoadSimulationsFromSnapshot", normalizePath(snapshotFile)),
    as.list(simulationNames)
  )
  netSimulations <- do.call(snapshotTask$call, args)

  simulations <- lapply(
    netSimulations,
    function(netSimulation) Simulation$new(netSimulation)
  )
  names(simulations) <- vapply(
    simulations,
    function(simulation) simulation$name,
    FUN.VALUE = character(1)
  )

  # When specific names were requested, check that all of them were found.
  if (!is.null(simulationNames)) {
    notFound <- setdiff(simulationNames, names(simulations))

    if (length(notFound) > 0 && !ignoreIfNotFound) {
      cli::cli_abort(
        message = c(
          "x" = "Simulation{?s} not found in the snapshot: {.val {notFound}}",
          "i" = "Set {.code ignoreIfNotFound = TRUE} to return {.code NULL} for missing simulations instead."
        )
      )
    }

    # Return one entry per requested name, in the requested order, with `NULL`
    # for the names that were not found.
    simulations <- stats::setNames(
      simulations[simulationNames],
      simulationNames
    )
  }

  simulations
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
