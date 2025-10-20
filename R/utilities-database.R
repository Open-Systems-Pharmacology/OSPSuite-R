#' Fix macOS SQLite database if needed
#'
#' On macOS, certain VIEWs with complex JOINs cause stack overflow in SQLite
#' due to limited P/Invoke stack size. This function converts those VIEWs to
#' TABLEs on first load and caches the result.
#'
#' @param dbPath Path to the PKSimDB.sqlite file
#' @return Logical indicating whether the fix was applied or already present
#' @keywords internal
.fixMacOSDatabaseIfNeeded <- function(dbPath) {
  # Only apply fix on macOS
  if (Sys.info()[["sysname"]] != "Darwin") {
    return(FALSE)
  }

  # Check for marker file to see if fix was already applied
  markerFile <- paste0(dbPath, ".macos-fixed")
  if (file.exists(markerFile)) {
    # Check if database has been updated since marker was created
    markerTime <- file.info(markerFile)$mtime
    dbTime <- file.info(dbPath)$mtime
    
    # If database is newer than marker, the database was updated - remove marker and recheck
    if (dbTime > markerTime) {
      message("PK-Sim database has been updated. Applying fix for macOS.")
      unlink(markerFile)
    } else {
      # Marker is valid and database hasn't changed
      return(FALSE)
    }
  }

  # Check if database actually needs fixing by querying sqlite_master
  needsFix <- tryCatch(
    {
      con <- RSQLite::dbConnect(RSQLite::SQLite(), dbPath)
      on.exit(RSQLite::dbDisconnect(con), add = TRUE)

      result <- RSQLite::dbGetQuery(
        con,
        "SELECT type FROM sqlite_master WHERE name = 'ContainerParameters_Species'"
      )

      if (nrow(result) == 0) {
        warning("ContainerParameters_Species not found in database")
        return(FALSE)
      }

      # If it's already a TABLE, just create marker and return
      if (result$type[1] == "table") {
        writeLines(
          c(
            "This marker file indicates that the macOS SQLite fix has been applied.",
            paste("Marker created on:", Sys.time()),
            "The PKSimDB.sqlite file has been modified to convert problematic VIEWs to TABLEs.",
            "",
            "This marker will be automatically removed if the database is updated (based on file modification time)."
          ),
          markerFile
        )
        return(FALSE)
      }

      # It's a VIEW, needs fixing
      return(TRUE)
    },
    error = function(e) {
      warning("Could not check database status: ", e$message)
      return(FALSE)
    }
  )

  if (!needsFix) {
    return(FALSE)
  }

  # Apply the fix
  message("Applying macOS SQLite fix to PKSimDB.sqlite (first load only)...")

  tryCatch(
    {
      con <- RSQLite::dbConnect(RSQLite::SQLite(), dbPath)
      on.exit(RSQLite::dbDisconnect(con), add = TRUE)

      # Drop and recreate ContainerParameters_Species as TABLE
      RSQLite::dbExecute(
        con,
        "DROP VIEW IF EXISTS [ContainerParameters_Species]"
      )
      RSQLite::dbExecute(
        con,
        "DROP TABLE IF EXISTS [ContainerParameters_Species]"
      )

      RSQLite::dbExecute(
        con,
        "CREATE TABLE [ContainerParameters_Species] (
          [container_id] INTEGER NOT NULL,
          [container_type] TEXT NOT NULL,
          [container_name] TEXT NOT NULL,
          [parameter_name] TEXT NOT NULL,
          [species] TEXT NOT NULL
        )"
      )

      RSQLite::dbExecute(
        con,
        "INSERT INTO [ContainerParameters_Species]
        SELECT DISTINCT 
                        [cpv].[container_id], 
                        [cpv].[container_type], 
                        [cpv].[container_name], 
                        [cpv].[parameter_name], 
                        [species]
        FROM   [tab_container_parameter_values] AS [cpv],
               [tab_container_parameters] AS [cp]
        WHERE  [cpv].[container_id] = [cp].[container_id]
                 AND [cpv].[parameter_name] = [cp].[parameter_name]
                 AND [building_block_type] = 'INDIVIDUAL'
        UNION
        SELECT DISTINCT 
                        [cpr].[container_id], 
                        [cpr].[container_type], 
                        [cpr].[container_name], 
                        [cpr].[parameter_name], 
                        [species]
        FROM   [tab_container_parameter_rates] AS [cpr],
               [tab_species_calculation_methods] AS [scm],
               [tab_container_parameters] AS [cp]
        WHERE  [cpr].[calculation_method] = [scm].[calculation_method]
                 AND [cpr].[container_id] = [cp].[container_id]
                 AND [cpr].[parameter_name] = [cp].[parameter_name]
                 AND [building_block_type] = 'INDIVIDUAL'"
      )

      RSQLite::dbExecute(
        con,
        "CREATE INDEX [idx_container_params_species] 
          ON [ContainerParameters_Species]([species], [container_id], [parameter_name])"
      )

      # Fix VIEW_INDIVIDUAL_PARAMETER_SAME_FORMULA_OR_VALUE_FOR_ALL_SPECIES
      RSQLite::dbExecute(
        con,
        "DROP VIEW IF EXISTS [VIEW_INDIVIDUAL_PARAMETER_SAME_FORMULA_OR_VALUE_FOR_ALL_SPECIES]"
      )
      RSQLite::dbExecute(
        con,
        "DROP TABLE IF EXISTS [VIEW_INDIVIDUAL_PARAMETER_SAME_FORMULA_OR_VALUE_FOR_ALL_SPECIES]"
      )

      RSQLite::dbExecute(
        con,
        "CREATE TABLE [VIEW_INDIVIDUAL_PARAMETER_SAME_FORMULA_OR_VALUE_FOR_ALL_SPECIES] (
          [ContainerId] INTEGER NOT NULL,
          [ContainerType] TEXT NOT NULL,
          [ContainerName] TEXT NOT NULL,
          [ParameterName] TEXT NOT NULL,
          [IsSameFormula] INTEGER NOT NULL
        )"
      )

      RSQLite::dbExecute(
        con,
        "INSERT INTO [VIEW_INDIVIDUAL_PARAMETER_SAME_FORMULA_OR_VALUE_FOR_ALL_SPECIES]
        SELECT DISTINCT 
                        [tab_container_parameter_rates].[container_id] AS [ContainerId], 
                        [tab_container_parameter_rates].[container_type] AS [ContainerType], 
                        [tab_container_parameter_rates].[container_name] AS [ContainerName], 
                        [tab_container_parameter_rates].[parameter_name] AS [ParameterName], 
                        1 AS [IsSameFormula]
        FROM   [tab_container_parameter_rates],
               [tab_species_calculation_methods],
               [tab_container_parameters]
        WHERE  [tab_container_parameter_rates].[calculation_method] = [tab_species_calculation_methods].[calculation_method]
                 AND [tab_container_parameters].[container_id] = [tab_container_parameter_rates].[container_id]
                 AND [tab_container_parameters].[container_type] = [tab_container_parameter_rates].[container_type]
                 AND [tab_container_parameters].[container_name] = [tab_container_parameter_rates].[container_name]
                 AND [tab_container_parameters].[parameter_name] = [tab_container_parameter_rates].[parameter_name]
                 AND [tab_container_parameters].[building_block_type] = 'INDIVIDUAL'
        GROUP  BY
                  [tab_container_parameter_rates].[container_id], 
                  [tab_container_parameter_rates].[container_type], 
                  [tab_container_parameter_rates].[container_name], 
                  [tab_container_parameter_rates].[parameter_name], 
                  [tab_container_parameter_rates].[calculation_method], 
                  [tab_container_parameter_rates].[formula_rate]
        HAVING COUNT ([tab_species_calculation_methods].[species]) = (SELECT COUNT ([species]) FROM [tab_species])
        UNION
        SELECT DISTINCT 
                        [tab_container_parameter_values].[container_id] AS [ContainerId], 
                        [tab_container_parameter_values].[container_type] AS [ContainerType], 
                        [tab_container_parameter_values].[container_name] AS [ContainerName], 
                        [tab_container_parameter_values].[parameter_name] AS [ParameterName], 
                        0 AS [IsSameFormula]
        FROM   [tab_container_parameters],
               [tab_container_parameter_values],
               [tab_species_parameter_value_versions]
        WHERE  [tab_container_parameters].[container_id] = [tab_container_parameter_values].[container_id]
                 AND [tab_container_parameters].[container_type] = [tab_container_parameter_values].[container_type]
                 AND [tab_container_parameters].[container_name] = [tab_container_parameter_values].[container_name]
                 AND [tab_container_parameters].[parameter_name] = [tab_container_parameter_values].[parameter_name]
                 AND [tab_species_parameter_value_versions].[species] = [tab_container_parameter_values].[species]
                 AND [tab_species_parameter_value_versions].[parameter_value_version] = [tab_container_parameter_values].[parameter_value_version]
                 AND [tab_container_parameters].[building_block_type] = 'INDIVIDUAL'
        GROUP  BY
                  [tab_container_parameter_values].[container_id], 
                  [tab_container_parameter_values].[container_type], 
                  [tab_container_parameter_values].[container_name], 
                  [tab_container_parameter_values].[parameter_name], 
                  [tab_container_parameter_values].[default_value]
        HAVING COUNT ([tab_container_parameter_values].[species]) = (SELECT COUNT ([species]) FROM [tab_species])"
      )

      RSQLite::dbExecute(
        con,
        "CREATE INDEX [idx_same_formula_species] 
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

      message("macOS SQLite fix applied successfully")
      return(TRUE)
    },
    error = function(e) {
      stop("Failed to apply macOS SQLite fix: ", e$message)
    }
  )
}
