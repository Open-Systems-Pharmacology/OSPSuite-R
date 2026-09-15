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
    object = c(exportCSV, exportPKML, exportJSON, exportXML, RunForAllOutputs)
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

#' Convert snapshots to project files
#'
#' @description
#' Converts one or more snapshot files (`.json`) into project files and writes
#' them to an output directory.
#'
#' PK-Sim (`.pksim5`) and MoBi (`.mbp3`) projects are both supported. The
#' application that wrote a snapshot is detected from the snapshot itself; use
#' `application` to force the choice.
#'
#' @param ... character strings, path to snapshot files (`.json`) or a directory
#'   containing snapshot files to convert.
#' @param output character string, path to the output directory where to write
#'   the converted project files.
#' @param runSimulations logical, whether to run the simulations during
#'   conversion (default = `FALSE`).
#' @param application character string, `"PK-Sim"` or `"MoBi"`, the application
#'   whose project files are written. If `NULL` (default), it is detected for
#'   each snapshot separately.
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' snapshotToProject("path/to/snapshot.json", output = "path/to/output")
#' }
snapshotToProject <- function(
  ...,
  output = ".",
  runSimulations = FALSE,
  application = NULL
) {
  inputs <- c(...)
  if (!is.null(application)) {
    rlang::arg_match(arg = application, values = c("PK-Sim", "MoBi"))
  }
  .validateSnapshotConversionInputs(
    inputs = inputs,
    output = output,
    runSimulations = runSimulations
  )

  runs <- .splitByApplication(
    inputFolder = .gatherFiles(inputs),
    pattern = "\\.json$",
    what = "snapshot files (.json)",
    application = application,
    detect = .snapshotApplicationFromSnapshot
  )

  for (run in runs) {
    .runSnapshotConversion(
      inputFolder = run$folder,
      output = output,
      exportMode = "Project",
      runSimulations = runSimulations,
      nfiles = run$nfiles,
      application = run$application,
      targetFormat = "project"
    )
  }

  invisible(NULL)
}

#' Convert project files to snapshots
#'
#' @description
#' Converts one or more project files into snapshot files (`.json`) and writes
#' them to an output directory.
#'
#' PK-Sim (`.pksim5`) and MoBi (`.mbp3`) projects are both supported. The
#' application is detected from the file extension; use `application` to force
#' the choice.
#'
#' @param ... character strings, path to project files (`.pksim5` or `.mbp3`) or
#'   a directory containing project files to convert.
#' @param output character string, path to the output directory where to write
#'   the converted snapshot files.
#' @param application character string, `"PK-Sim"` or `"MoBi"`, the application
#'   the project files belong to. If `NULL` (default), it is detected for each
#'   project separately.
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' projectToSnapshot("path/to/project.pksim5", output = "path/to/output")
#' projectToSnapshot("path/to/project.mbp3", output = "path/to/output")
#' }
projectToSnapshot <- function(..., output = ".", application = NULL) {
  inputs <- c(...)
  if (!is.null(application)) {
    rlang::arg_match(arg = application, values = c("PK-Sim", "MoBi"))
  }
  .validateSnapshotConversionInputs(inputs = inputs, output = output)

  runs <- .splitByApplication(
    inputFolder = .gatherFiles(inputs),
    pattern = "\\.(pksim5|mbp3)$",
    what = "project files (.pksim5 or .mbp3)",
    application = application,
    detect = .snapshotApplicationFromProject
  )

  for (run in runs) {
    .runSnapshotConversion(
      inputFolder = run$folder,
      output = output,
      exportMode = "Snapshot",
      runSimulations = FALSE,
      nfiles = run$nfiles,
      application = run$application,
      targetFormat = "snapshot"
    )
  }

  invisible(NULL)
}

#' Convert between snapshot and project formats
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `convertSnapshot()` is deprecated and will be removed in a future release.
#' Use [snapshotToProject()] to convert a snapshot to a project, and
#' [projectToSnapshot()] to convert a project to a snapshot.
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
      when = "13.0.0",
      what = "convertSnapshot()",
      with = "snapshotToProject()"
    )
    snapshotToProject(
      ...,
      output = output,
      runSimulations = runSimulations
    )
  } else {
    lifecycle::deprecate_warn(
      when = "13.0.0",
      what = "convertSnapshot()",
      with = "projectToSnapshot()"
    )
    projectToSnapshot(..., output = output)
  }
}

#' Detect which application a project file belongs to
#'
#' @param files character vector, paths to project files.
#'
#' @return character vector of application names, one per element of `files`.
#' @keywords internal
#' @noRd
.snapshotApplicationFromProject <- function(files) {
  ifelse(grepl("\\.mbp3$", files, ignore.case = TRUE), "MoBi", "PK-Sim")
}

#' Detect which application wrote a snapshot file
#'
#' MoBi records its name in the `ApplicationName` field of the snapshot, right
#' next to `Version` at the top of the file. PK-Sim writes no such field.
#'
#' @param files character vector, paths to snapshot files (`.json`).
#'
#' @return character vector of application names, one per element of `files`.
#' @keywords internal
#' @noRd
.snapshotApplicationFromSnapshot <- function(files) {
  vapply(
    files,
    function(file) {
      # Scan the whole file rather than a prefix: MoBi writes `ApplicationName`
      # next to `Version` at the top today, but a snapshot that carried it
      # further down would otherwise be handed to the PK-Sim core silently.
      # A full JSON parser would answer the same question at the cost of a new
      # dependency and of materialising a multi-megabyte snapshot as R objects.
      contents <- readChar(file, nchars = file.size(file), useBytes = TRUE)
      if (grepl('"ApplicationName"\\s*:\\s*"MoBi"', contents)) {
        "MoBi"
      } else {
        "PK-Sim"
      }
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
}

#' Group gathered files by the application that has to convert them
#'
#' @param inputFolder character string, folder returned by [.gatherFiles()].
#' @param pattern regular expression matching the files to convert.
#' @param what character string naming the expected files, used in the error
#'   message raised when `inputFolder` holds none of them.
#' @param application character string or `NULL`. When `NULL`, the application is
#'   detected for each file; otherwise every file is assigned to `application`.
#' @param detect function taking file paths and returning one application name
#'   per path. Not called when `application` is given.
#'
#' @return A list with one element per conversion run, each a list of
#'   `application`, `folder` and `nfiles`.
#' @keywords internal
#' @noRd
.splitByApplication <- function(
  inputFolder,
  pattern,
  what,
  application,
  detect
) {
  files <- list.files(
    inputFolder,
    pattern = pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(files) == 0L) {
    cli::cli_abort(
      message = c("x" = "No {what} found in the input paths.")
    )
  }

  # Converted files are named after the input, so two inputs sharing a name
  # (`model.pksim5` and `model.mbp3`) would write the same output file, the
  # second silently overwriting the first. Names are compared case-insensitively
  # so the same batch is accepted or rejected on every file system.
  stems <- tolower(sub("\\.[^.]*$", "", basename(files)))
  collisions <- unique(stems[duplicated(stems)])
  if (length(collisions) > 0L) {
    colliding <- basename(files)[stems %in% collisions]
    cli::cli_abort(
      message = c(
        "x" = "Some input files would be converted to the same output file: {.file {colliding}}.",
        "i" = "Rename one of them, or convert them in separate calls."
      )
    )
  }

  applications <- if (is.null(application)) {
    detect(files)
  } else {
    rep(application, length(files))
  }

  # One application covers every file: convert the gathered folder as it is.
  if (length(unique(applications)) == 1L) {
    return(list(list(
      application = applications[[1]],
      folder = inputFolder,
      nfiles = length(files)
    )))
  }

  # A mixed batch needs one folder, and one conversion run, per application.
  lapply(unique(applications), function(app) {
    appFiles <- files[applications == app]
    folder <- tempfile()
    dir.create(folder)
    file.rename(appFiles, file.path(folder, basename(appFiles)))
    list(application = app, folder = folder, nfiles = length(appFiles))
  })
}

#' Get a `SnapshotExportMode` enum value from .NET
#'
#' `rSharp` does not marshal an R integer into an enum parameter, and the MoBi
#' entry point takes the export mode as a method argument, so a real enum object
#' is needed. It is also accepted by the PK-Sim run options property, which lets
#' both cores share one representation.
#'
#' @param name character string, `"Project"` or `"Snapshot"`.
#'
#' @return A `NetObject` wrapping the enum value.
#' @keywords internal
#' @noRd
.snapshotExportMode <- function(name) {
  rSharp::callStatic(
    "System.Enum, System.Runtime",
    "Parse",
    rSharp::getType(
      "OSPSuite.CLI.Core.Services.SnapshotExportMode, OSPSuite.CLI.Core"
    ),
    name
  )
}

#' Run a snapshot/project conversion via the PK-Sim or MoBi core
#'
#' @param inputFolder character string, directory holding the files to convert.
#' @param output character string, output directory for the converted files.
#' @param exportMode character string, `"Project"` for snapshot -> project or
#'   `"Snapshot"` for project -> snapshot. A member of the
#'   `OSPSuite.CLI.Core.Services.SnapshotExportMode` enum.
#' @param runSimulations logical, whether to run simulations during conversion.
#' @param nfiles integer, number of input files (used for the progress message).
#' @param application character string, `"PK-Sim"` or `"MoBi"`, the core to run.
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
  application,
  targetFormat
) {
  # Attribute conversion errors to the public function that called this helper
  # (snapshotToProject/projectToSnapshot). Forced here, not in the error
  # handler, so it resolves to this frame's caller rather than the handler.
  call <- rlang::caller_env()

  exportMode <- .snapshotExportMode(exportMode)

  cli::cli_process_start(
    msg = "Converting {nfiles} {application} file{?s} to {targetFormat} format",
    msg_done = "Conversion completed",
    msg_failed = "An error occured while converting files"
  )

  tryCatch(
    {
      if (application == "MoBi") {
        # MoBi exposes no static entry point for this; the method lives on the
        # snapshot task. `MoBi.R` is initialised with the package, and its
        # snapshot converter reflects into the co-located `PKSim.R.dll` by
        # itself when a snapshot carries PK-Sim modules, so `initPKSim()` is
        # not needed here.
        #
        # `RunSnapshot(string inputFolder, string outputFolder,
        #   bool runSimulations, SnapshotExportMode exportMode,
        #   params string[] folders)`; the trailing array is left empty.
        snapshotTask <- .getMoBiTaskFromCache("SnapshotTask")
        invisible(snapshotTask$call(
          "RunSnapshot",
          inputFolder,
          normalizePath(output),
          isTRUE(runSimulations),
          exportMode
        ))
      } else {
        initPKSim()

        SnapshotRunOptions <- rSharp::newObjectFromName(
          "OSPSuite.CLI.Core.RunOptions.SnapshotRunOptions"
        )
        SnapshotRunOptions$set(name = "InputFolder", value = inputFolder)
        SnapshotRunOptions$set(
          name = "OutputFolder",
          value = normalizePath(output)
        )
        SnapshotRunOptions$set(
          name = "RunSimulations",
          value = isTRUE(runSimulations)
        )
        SnapshotRunOptions$set("ExportMode", exportMode)

        invisible(rSharp::callStatic(
          "PKSim.R.Api, PKSim.R",
          "RunSnapshot",
          SnapshotRunOptions
        ))
      }
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
  files <- unlist(lapply(c(...), function(element) {
    # a folder contributes every file it holds, at any depth
    if (dir.exists(element)) {
      return(list.files(element, full.names = TRUE, recursive = TRUE))
    }
    if (file.exists(element)) {
      return(element)
    }
    character(0)
  }))

  # Every file lands directly in one folder, so two inputs sharing a name would
  # collapse into one and the second would be processed in place of the first.
  # Names are compared case-insensitively so the same inputs are accepted or
  # rejected on every file system.
  names <- tolower(basename(files))
  duplicates <- unique(names[duplicated(names)])
  if (length(duplicates) > 0L) {
    cli::cli_abort(
      message = c(
        "x" = "Some input files share a name and would overwrite each other: {.file {basename(files)[names %in% duplicates]}}.",
        "i" = "Rename one of them, or process them in separate calls."
      )
    )
  }

  temp_dir <- tempfile()
  dir.create(temp_dir)
  if (length(files) > 0L) {
    file.copy(from = files, to = temp_dir)
  }

  temp_dir
}
