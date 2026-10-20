#' @title Calculates the pkAnalyses for all output values available in `results`.
#'
#' @param results Results of simulation. Typically the `results` are calculated
#'   using `runSimulations` or imported from csv file via `importResults`.
#'
#' @return An instance of `SimulationPKAnalyses` class.
#'
#' @examples
#'
#' simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' addOutputs("Organism|VenousBlood|*|Aciclovir", sim)
#' results <- runSimulations(sim)[[1]]
#' pkAnalyses <- calculatePKAnalyses(results)
#' @export
calculatePKAnalyses <- function(results) {
  validateIsOfType(results, "SimulationResults")
  pkAnalysisTask <- .getCoreTask("PKAnalysisTask")
  calculatePKAnalysisArgs <- rSharp::newObjectFromName(
    "OSPSuite.R.Services.CalculatePKAnalysisArgs"
  )
  calculatePKAnalysisArgs$set("Simulation", results$simulation)
  calculatePKAnalysisArgs$set("SimulationResults", results)
  pkAnalyses <- pkAnalysisTask$call("CalculateFor", calculatePKAnalysisArgs)
  SimulationPKAnalyses$new(pkAnalyses, results$simulation)
}

#' @title Saves the pK-analyses  to csv file
#'
#' @param pkAnalyses pK-Analyses to export (typically calculated using `calculatePKAnalyses` or imported from file)
#' @param filePath Full path where the pK-Analyses will be saved.
#'
#' @export
exportPKAnalysesToCSV <- function(pkAnalyses, filePath) {
  validateIsOfType(pkAnalyses, "SimulationPKAnalyses")
  validateIsString(filePath)
  filePath <- path.expand(filePath)
  pkAnalysisTask <- .getCoreTask("PKAnalysisTask")
  pkAnalysisTask$call(
    "ExportPKAnalysesToCSV",
    pkAnalyses,
    pkAnalyses$simulation,
    filePath
  )
  invisible()
}

#' @inherit exportPKAnalysesToCSV
.savePKAnalysesToCSV <- function(pkAnalyses, filePath) {
  exportPKAnalysesToCSV(pkAnalyses, filePath)
}

#' @title Loads the pK-analyses from csv file
#'
#' @param filePath Full path of the file containing the pK-Analyses to load.
#' @param simulation Instance of the simulation for which the pk-Analyses were
#'   calculated. This is required to verify that the file matches the simulation.
#'
#' @export
importPKAnalysesFromCSV <- function(filePath, simulation) {
  validateIsOfType(simulation, "Simulation")
  validateIsString(filePath)
  filePath <- path.expand(filePath)
  pkAnalysisTask <- .getCoreTask("PKAnalysisTask")
  pkAnalyses <- pkAnalysisTask$call(
    "ImportPKAnalysesFromCSV",
    filePath,
    simulation
  )
  SimulationPKAnalyses$new(pkAnalyses, simulation)
}


#' @title Convert the pk-Analysis to data frame
#'
#' @description Optionally, only some outputs and PK parameters are kept.
#'
#' @param pkAnalyses pK-Analyses to convert to data frame (typically calculated
#'   using `calculatePKAnalyses` or imported from file).
#' @param quantitiesOrPaths Optional. Quantity instances or paths of the
#'   outputs to include. `NULL` (default) includes all outputs. Each path must
#'   be one of `pkAnalyses$allQuantityPaths`, otherwise an error is thrown.
#'   Wildcards (`*`) are not accepted.
#' @param pkParameters Optional. Names of the PK parameters to include, e.g.
#'   `c("C_max", "AUC_inf")`. `NULL` (default) includes all PK parameters. Each
#'   name must be one of `pkAnalyses$allPKParameterNames`, otherwise an error is
#'   thrown. Names are case sensitive.
#'
#' @return A tibble with one row per individual, output, and PK parameter, and
#'   the columns `IndividualId`, `QuantityPath`, `Parameter`, `Value`, and
#'   `Unit`. Values that could not be calculated are `NA`. For PK-Analyses
#'   from `calculatePKAnalyses()`, a selection keeps the row order of the full
#'   result. For imported PK-Analyses, the selected rows are grouped by output
#'   in the order of `pkAnalyses$allQuantityPaths`, unless all outputs and PK
#'   parameters are selected. If no row matches the selection, the result has
#'   zero rows.
#'
#' @examples
#' simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' addOutputs("Organism|VenousBlood|*|Aciclovir", sim)
#' results <- runSimulations(sim)[[1]]
#' pkAnalyses <- calculatePKAnalyses(results)
#'
#' # All PK parameters of all outputs
#' pkAnalysesToDataFrame(pkAnalyses)
#'
#' # Only C_max and AUC_inf of the venous plasma concentration
#' pkAnalysesToDataFrame(
#'   pkAnalyses,
#'   quantitiesOrPaths = "Organism|VenousBlood|Plasma|Aciclovir",
#'   pkParameters = c("C_max", "AUC_inf")
#' )
#' @export
pkAnalysesToDataFrame <- function(
  pkAnalyses,
  quantitiesOrPaths = NULL,
  pkParameters = NULL
) {
  validateIsOfType(pkAnalyses, "SimulationPKAnalyses")
  validateIsOfType(
    quantitiesOrPaths,
    c("Quantity", "character"),
    nullAllowed = TRUE
  )
  validateIsCharacter(pkParameters, nullAllowed = TRUE)

  selection <- .selectPKAnalyses(
    pkAnalyses,
    quantitiesOrPaths = quantitiesOrPaths,
    pkParameters = pkParameters
  )

  netPKAnalyses <- pkAnalyses
  if (!is.null(selection) && selection$filterInNet) {
    netPKAnalyses <- .filterPKAnalysesInNet(
      pkAnalyses,
      quantityPaths = selection$quantityPaths,
      pkParameters = selection$pkParameters
    )
  }

  pkAnalysisTask <- .getCoreTask("PKAnalysisTask")
  # The whole table is passed as one string (rSharp has no converter for a .NET
  # DataTable). A .NET string holds at most about 1e9 characters, about 14
  # million rows with the paths of Aciclovir; in practice memory runs out first
  csv <- pkAnalysisTask$call(
    "PKAnalysesToCSV",
    netPKAnalyses,
    pkAnalyses$simulation
  )
  pkAnalysesData <- .pkAnalysesCSVToDataFrame(csv)

  if (!is.null(selection) && !selection$filterInNet) {
    keep <- pkAnalysesData$QuantityPath %in%
      selection$quantityPaths &
      pkAnalysesData$Parameter %in% selection$pkParameters
    keptRows <- which(keep)
    # Group the rows by output in the order of `allQuantityPaths`, like the
    # .NET route. `order()` is stable, so the order within an output is kept.
    outputOrder <- order(
      match(pkAnalysesData$QuantityPath[keptRows], selection$quantityPaths)
    )
    pkAnalysesData <- pkAnalysesData[keptRows[outputOrder], ]
  }

  return(pkAnalysesData)
}

#' @rdname pkAnalysesToDataFrame
#'
#' @export
pkAnalysesToTibble <- function(
  pkAnalyses,
  quantitiesOrPaths = NULL,
  pkParameters = NULL
) {
  pkAnalysesData <- pkAnalysesToDataFrame(
    pkAnalyses,
    quantitiesOrPaths = quantitiesOrPaths,
    pkParameters = pkParameters
  )

  # consistently return a tibble data frame
  return(dplyr::as_tibble(pkAnalysesData))
}

#' Validate the selection of outputs and PK parameters
#'
#' @param pkAnalyses A `SimulationPKAnalyses` object.
#' @param quantitiesOrPaths Quantities or paths to keep, or `NULL` for all.
#' @param pkParameters Names of the PK parameters to keep, or `NULL` for all.
#'
#' @returns `NULL` when the selection covers all outputs and PK parameters.
#'   Otherwise a list with `quantityPaths` and `pkParameters` (the selected
#'   paths and names, never `NULL`) and `filterInNet` (whether filtering in
#'   .NET is expected to be faster than filtering the data frame in R).
#' @noRd
.selectPKAnalyses <- function(pkAnalyses, quantitiesOrPaths, pkParameters) {
  if (is.null(quantitiesOrPaths) && is.null(pkParameters)) {
    return(NULL)
  }

  allPaths <- pkAnalyses$allQuantityPaths
  allPKParameterNames <- pkAnalyses$allPKParameterNames

  quantityPaths <- allPaths
  if (!is.null(quantitiesOrPaths)) {
    requestedPaths <- .entitiesToPaths(c(quantitiesOrPaths))
    missingPaths <- setdiff(requestedPaths, allPaths)
    if (length(missingPaths) > 0) {
      stop(
        messages$errorPKAnalysesQuantityPathsNotFound(missingPaths),
        call. = FALSE
      )
    }
    quantityPaths <- allPaths[allPaths %in% requestedPaths]
  }

  selectedPKParameters <- allPKParameterNames
  if (!is.null(pkParameters)) {
    missingPKParameters <- setdiff(pkParameters, allPKParameterNames)
    if (length(missingPKParameters) > 0) {
      stop(
        messages$errorPKAnalysesPKParametersNotFound(missingPKParameters),
        call. = FALSE
      )
    }
    selectedPKParameters <- allPKParameterNames[
      allPKParameterNames %in% pkParameters
    ]
  }

  numberOfSelected <- length(quantityPaths) * length(selectedPKParameters)
  numberOfAll <- length(allPaths) * length(allPKParameterNames)
  if (numberOfSelected == numberOfAll) {
    return(NULL)
  }

  # Nothing selected: an empty .NET object gives the empty table fastest
  if (numberOfSelected == 0) {
    return(list(
      quantityPaths = quantityPaths,
      pkParameters = selectedPKParameters,
      filterInNet = TRUE
    ))
  }

  firstPKParameter <- pkAnalyses$call(
    "PKParameterFor",
    allPaths[[1]],
    pkAnalyses$call("AllPKParameterNamesFor", allPaths[[1]])[[1]]
  )
  numberOfIndividuals <- firstPKParameter$get("Count")

  return(list(
    quantityPaths = quantityPaths,
    pkParameters = selectedPKParameters,
    filterInNet = .isFilterInNetFaster(
      numberOfSelected = numberOfSelected,
      numberOfAll = numberOfAll,
      numberOfIndividuals = numberOfIndividuals
    )
  ))
}

#' Decide whether filtering PK-Analyses in .NET is faster than in R
#'
#' @details Filtering in .NET costs about 0.5 ms per selected PK parameter of
#'   one output (two calls through rSharp). Converting and filtering in R costs
#'   about 2.5 microseconds per row (one row per individual) that is converted in
#'   vain.
#'   Measured on Aciclovir with 1 to 500 individuals and 4 to 133 outputs.
#'   Near the threshold, a wrong choice costs up to about 25%. This rule and
#'   the R route can be removed once `PKAnalysesToCSV` can filter in a single
#'   call (OSPSuite.Core issue #3022).
#'
#' @param numberOfSelected Number of selected output and PK parameter
#'   combinations (upper bound).
#' @param numberOfAll Number of all output and PK parameter combinations
#'   (upper bound).
#' @param numberOfIndividuals Number of values per combination.
#'
#' @returns `TRUE` if filtering in .NET is expected to be faster.
#' @noRd
.isFilterInNetFaster <- function(
  numberOfSelected,
  numberOfAll,
  numberOfIndividuals
) {
  costPerSelectedInNet <- 0.5e-3
  costPerRowInR <- 2.5e-6
  numberOfSelected * costPerSelectedInNet <
    (numberOfAll - numberOfSelected) * numberOfIndividuals * costPerRowInR
}

#' Build a .NET `PopulationSimulationPKAnalyses` with selected PK parameters
#'
#' @param pkAnalyses A `SimulationPKAnalyses` object.
#' @param quantityPaths Paths of the outputs to keep.
#' @param pkParameters Names of the PK parameters to keep.
#'
#' @returns A new .NET `PopulationSimulationPKAnalyses` holding the selected
#'   `QuantityPKParameter` objects of `pkAnalyses` (the same objects, not
#'   copies). `pkAnalyses` is not modified. The objects are added output by
#'   output in the order of `quantityPaths`, and within an output in the order
#'   of `pkAnalyses`. The R route of `pkAnalysesToDataFrame()` orders its rows
#'   the same way.
#' @noRd
.filterPKAnalysesInNet <- function(pkAnalyses, quantityPaths, pkParameters) {
  filteredPKAnalyses <- rSharp::newObjectFromName(
    "OSPSuite.Core.Domain.PopulationSimulationPKAnalyses"
  )
  for (path in quantityPaths) {
    names <- pkAnalyses$call("AllPKParameterNamesFor", path)
    for (name in names[names %in% pkParameters]) {
      filteredPKAnalyses$call(
        "AddPKAnalysis",
        pkAnalyses$call("PKParameterFor", path, name)
      )
    }
  }

  return(filteredPKAnalyses)
}

#' Parse the CSV text of PK-Analyses created by .NET
#'
#' @param csv A single string as returned by `PKAnalysisTask.PKAnalysesToCSV`,
#'   with the columns `IndividualId`, `QuantityPath`, `Parameter`, `Value`, and
#'   `Unit`.
#'
#' @details The text is split into columns in memory with
#'   `data.table::fread()`. `readr::read_csv()` is not used because it writes
#'   literal input to a temporary file first. `Value` is converted with
#'   `readr::parse_double()` with the default locale (not the option
#'   `readr.default_locale`), which gives the same doubles as the former
#'   file-based conversion with `readr::read_csv()`; `as.numeric()` and
#'   `fread()` differ in the last digit for some values. `NaN`, `Infinity`,
#'   `-Infinity`, `NA`, and empty values become `NA`.
#'
#' @returns A tibble with typed columns, also when `csv` has no data rows.
#' @noRd
.pkAnalysesCSVToDataFrame <- function(csv) {
  pkAnalysesData <- data.table::fread(
    text = csv,
    sep = ",",
    header = TRUE,
    # `Value` is read as text and converted below: fread would keep the whole
    # column as text if one value is not a number
    colClasses = c(
      "integer",
      "character",
      "character",
      "character",
      "character"
    ),
    na.strings = c("NaN", "", "NA", "Infinity", "-Infinity"),
    encoding = "UTF-8",
    showProgress = FALSE
  )
  # Text that is not a number becomes NA with a warning
  values <- readr::parse_double(
    pkAnalysesData$Value,
    locale = readr::locale()
  )
  attr(values, "problems") <- NULL
  pkAnalysesData$Value <- values

  return(dplyr::as_tibble(pkAnalysesData))
}
