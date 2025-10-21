#' Fix macOS ARM64 SQLite database if needed
#'
#' On macOS ARM64 (Apple Silicon), certain VIEWs with complex JOINs cause stack overflow in SQLite
#' due to limited P/Invoke stack size. This function converts those VIEWs to
#' TABLEs on first load and caches the result.
#'
#' @param dbPath Path to the PKSimDB.sqlite file
#' @return Logical indicating whether the fix was applied or already present
#' @keywords internal
.fixMacOSDatabaseIfNeeded <- function(dbPath) {
  # Only apply fix on macOS ARM64 (Apple Silicon)
  if (Sys.info()[["sysname"]] != "Darwin") {
    return(FALSE)
  }

  # Check if we're on ARM64 (Apple Silicon) architecture
  if (!Sys.info()[["machine"]] %in% c("arm64", "aarch64")) {
    return(FALSE)
  }

  # Check if RSQLite is available (it's in Suggests)
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    cli::cli_abort(
      message = c(
        "x" = "RSQLite package is required to use {.fn runSimulationsFromSnapshot} on this platform (macOS ARM64).",
        "i" = "Install with: {.run install.packages('RSQLite')}"
      )
    )
    return(FALSE)
  }

  # Check for marker file to see if fix was already applied
  markerFile <- paste0(dbPath, ".macos-fixed")
  backupPath <- paste0(dbPath, ".original")

  if (file.exists(markerFile)) {
    # Check if database has been updated since marker was created
    markerTime <- file.info(markerFile)$mtime
    dbTime <- file.info(dbPath)$mtime

    # If database is newer than marker, the database was updated - remove marker and backup
    if (dbTime > markerTime) {
      unlink(markerFile)
      # Remove old backup so we create a fresh one
      if (file.exists(backupPath)) {
        unlink(backupPath)
      }
    } else {
      # Marker is valid and database hasn't changed
      return(FALSE)
    }
  }

  # Apply the fix
  # Create backup of original database before applying fix (for testing)
  if (!file.exists(backupPath)) {
    file.copy(dbPath, backupPath, overwrite = FALSE)
  }

  tryCatch(
    {
      con <- RSQLite::dbConnect(RSQLite::SQLite(), dbPath)
      on.exit(RSQLite::dbDisconnect(con), add = TRUE)

      # Convert both problematic VIEWs to TABLEs
      .convertViewToTable(con, "ContainerParameters_Species")
      .convertViewToTable(
        con,
        "VIEW_INDIVIDUAL_PARAMETER_SAME_FORMULA_OR_VALUE_FOR_ALL_SPECIES"
      )

      # Create indexes for performance
      RSQLite::dbExecute(
        con,
        "CREATE INDEX IF NOT EXISTS [idx_container_params_species]
          ON [ContainerParameters_Species]([species], [container_id], [parameter_name])"
      )

      RSQLite::dbExecute(
        con,
        "CREATE INDEX IF NOT EXISTS [idx_same_formula_species]
          ON [VIEW_INDIVIDUAL_PARAMETER_SAME_FORMULA_OR_VALUE_FOR_ALL_SPECIES]([IsSameFormula], [ContainerId], [ParameterName])"
      )

      # Optimize database
      RSQLite::dbExecute(con, "VACUUM")

      # Create marker file to indicate fix was applied
      writeLines(
        c(
          "This marker file indicates that the macOS SQLite fix has been applied.",
          paste("Applied on:", Sys.time()),
          "The PKSimDB.sqlite file has been modified to convert problematic VIEWs to TABLEs.",
          "",
          "This marker will be automatically removed if the database is updated (based on file modification time)."
        ),
        markerFile
      )

      return(TRUE)
    },
    error = function(e) {
      stop("Failed to apply macOS SQLite fix: ", e$message)
    }
  )
}


#' Convert a SQLite VIEW to a TABLE in place
#'
#' Reads all data from a VIEW, drops the VIEW, and recreates it as a TABLE
#' with the same data. If the object is already a TABLE, does nothing.
#'
#' @param con SQLite database connection
#' @param viewName Name of the VIEW to convert
#' @keywords internal
.convertViewToTable <- function(con, viewName) {
  # Check if this is a VIEW or TABLE
  objectType <- RSQLite::dbGetQuery(
    con,
    sprintf(
      "SELECT type FROM sqlite_master WHERE name = '%s'",
      viewName
    )
  )$type[1]

  # If it's already a TABLE, nothing to do
  if (is.na(objectType)) {
    warning("Object ", viewName, " not found in database")
    return(FALSE)
  }

  if (objectType == "table") {
    # Already a table, no conversion needed
    return(FALSE)
  }

  if (objectType != "view") {
    warning("Object ", viewName, " is neither a VIEW nor a TABLE: ", objectType)
    return(FALSE)
  }

  # Read all data from the VIEW
  viewData <- RSQLite::dbReadTable(con, viewName)

  # Drop the VIEW
  RSQLite::dbExecute(con, sprintf("DROP VIEW [%s]", viewName))

  # Write data back as a TABLE
  RSQLite::dbWriteTable(con, viewName, viewData, overwrite = TRUE)

  return(TRUE)
}
