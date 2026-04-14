#' Convert an Initial Conditions Building Block to a data frame.
#'
#' @param initialConditionsBuildingBlock A `BuildingBlock` object of type `Initial Conditions`.
#'
#' @returns A data frame with the following columns:
#' - `Container Path`: Full path to the container where the molecule is located.
#' - `Molecule Name`: Name of the molecule.
#' - `Is Present`: Boolean indicating if the molecule is present.
#' - `Value`: Initial value of the molecule. For values that are defined by a formula, the return value can be `NaN`.
#' - `Unit`: Unit of the initial value.
#' - `Scale Divisor`: Scale divisor for the initial value.
#' - `Neg. Values Allowed`: Boolean indicating if negative values are allowed.
#'
#' @export
#'
#' @examples
#' sim <- loadSimulation(system.file(
#'   "extdata",
#'   "simple.pkml",
#'   package = "ospsuite"
#' ))
#' module <- sim$configuration$modules[[1]]
#' icBB <- module$getInitialConditionsBBs()[[1]]
#' df <- initialConditionsBBToDataFrame(icBB)
initialConditionsBBToDataFrame <- function(initialConditionsBuildingBlock) {
  .validateBuildingBlockType(
    initialConditionsBuildingBlock,
    BuildingBlockTypes$`Initial Conditions`
  )

  bbWithInitialConditionsToDataFrame(initialConditionsBuildingBlock)
}

#' Convert a building block containing initial conditions to a data frame.
#'
#' Works with any building block that contains initial conditions, including
#' `Initial Conditions` and `Expression Profile` building blocks.
#'
#' @param buildingBlock A `BuildingBlock` object of type `Initial Conditions`
#'   or `Expression Profile`.
#'
#' @returns A data frame with the following columns:
#' - `Container Path`: Full path to the container where the molecule is located.
#' - `Molecule Name`: Name of the molecule.
#' - `Is Present`: Boolean indicating if the molecule is present.
#' - `Value`: Initial value of the molecule. For values that are defined by a formula, the return value can be `NaN`.
#' - `Unit`: Unit of the initial value.
#' - `Scale Divisor`: Scale divisor for the initial value.
#' - `Neg. Values Allowed`: Boolean indicating if negative values are allowed.
#'
#' @export
bbWithInitialConditionsToDataFrame <- function(buildingBlock) {
  .validateBuildingBlockType(
    buildingBlock,
    c(
      BuildingBlockTypes$`Initial Conditions`,
      BuildingBlockTypes$`Expression Profile`
    )
  )

  icTask <- .getMoBiTaskFromCache("InitialConditionsTask")

  paths <- icTask$call("AllPathsFrom", buildingBlock)
  containerPaths <- vapply(
    paths,
    function(path) {
      .getParentContainerPath(path)
    },
    character(1),
    USE.NAMES = FALSE
  )
  moleculeNames <- vapply(
    paths,
    function(path) {
      tail(toPathArray(path), 1)
    },
    character(1),
    USE.NAMES = FALSE
  )
  isPresent <- icTask$call(
    "AllIsPresentFrom",
    buildingBlock,
    paths
  )
  values <- icTask$call(
    "AllValuesFrom",
    buildingBlock,
    paths
  )
  units <- icTask$call(
    "AllUnitsFrom",
    buildingBlock,
    paths
  )
  scaleDivisors <- icTask$call(
    "AllScaleDivisorsFrom",
    buildingBlock,
    paths
  )
  negativeValuesAllowed <- icTask$call(
    "AllNegativeValuesAllowedFrom",
    buildingBlock,
    paths
  )

  df <- data.frame(
    "Container Path" = containerPaths,
    "Molecule Name" = moleculeNames,
    "Is Present" = isPresent,
    "Value" = values,
    "Unit" = units,
    "Scale Divisor" = scaleDivisors,
    "Neg. Values Allowed" = negativeValuesAllowed,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  return(df)
}

#' Set or add initial conditions to an existing Initial Conditions building block.
#'
#' This functions allows adding or modifying initial condition entries. Only constant values are allowed. For setting or adding initial conditions defined by formulas, use the `setInitialConditionsFormulas` function.
#'
#' @param initialConditionsBuildingBlock A `BuildingBlock` object of type `Initial Conditions`.
#' The entries will be added to or set in this building block.
#' @param quantityPaths A list of full paths of the quantities (usually molecules). Should contain
#' all path elements and the molecule name, separated by `|`.
#' @param quantityValues A list of values for the quantities.
#' The length of this list should be equal to the length of `quantityPaths`. The values must be in base unit (µmol).
#' @param scaleDivisors Either a single value or a list of scale divisors for the quantities. By default, the value is set to 1 for all quantities.
#' If only single value is provided, the value will be set for all quantities.
#' If a list is provided, the length of this list should be equal to the length of `quantityPaths`.
#' @param isPresent Either a single value (`TRUE` or `FALSE`) or a list of boolean
#' values indicating whether the quantity is present or not. If a list is provided,
#' the length of this list should be equal to the length of `quantityPaths`.
#' @param negativeValuesAllowed A single boolean value or a list of boolean values
#' indicating whether negative values are allowed for the quantities. If a list is provided,
#' the length of this list should be equal to the length of `quantityPaths`.
#' @returns Invisibly returns the updated `initialConditionsBuildingBlock` object.
#' @export
#'
#' @examples
#' sim <- loadSimulation(system.file(
#'   "extdata",
#'   "simple.pkml",
#'   package = "ospsuite"
#' ))
#' module <- sim$configuration$modules[[1]]
#' icBB <- module$getInitialConditionsBBs()[[1]]
#' setInitialConditionsInBB(
#'   icBB,
#'   quantityPaths = c("Organ|Tissue|Molecule1", "Organ|Tissue|Molecule2"),
#'   quantityValues = c(1, 0),
#'   scaleDivisors = c(1, 10),
#'   isPresent = c(TRUE, FALSE),
#'   negativeValuesAllowed = c(TRUE, FALSE)
#' )
setInitialConditionsInBB <- function(
  initialConditionsBuildingBlock,
  quantityPaths,
  quantityValues,
  scaleDivisors = 1,
  isPresent = TRUE,
  negativeValuesAllowed = FALSE
) {
  .validateBuildingBlockType(
    initialConditionsBuildingBlock,
    BuildingBlockTypes$`Initial Conditions`
  )

  #Exit early if no quantity paths are provided
  if (length(quantityPaths) == 0) {
    return(invisible(initialConditionsBuildingBlock))
  }
  # if the scaleDivisors, isPresent or negativeValuesAllowed are provided as single values, replicate them to match the length of quantityPaths
  if (!is.null(scaleDivisors) && length(scaleDivisors) == 1) {
    scaleDivisors <- rep(scaleDivisors, length(quantityPaths))
  }

  if (!is.null(isPresent) && length(isPresent) == 1) {
    isPresent <- rep(isPresent, length(quantityPaths))
  }

  if (!is.null(negativeValuesAllowed) && length(negativeValuesAllowed) == 1) {
    negativeValuesAllowed <- rep(negativeValuesAllowed, length(quantityPaths))
  }

  if (
    length(quantityPaths) != length(quantityValues) ||
      (!is.null(scaleDivisors) &&
        length(quantityPaths) != length(scaleDivisors)) ||
      (!is.null(isPresent) && length(quantityPaths) != length(isPresent)) ||
      (!is.null(negativeValuesAllowed) &&
        length(quantityPaths) != length(negativeValuesAllowed))
  ) {
    stop(
      "The length of quantityPaths should be equal to the length of quantityValues, scaleDivisors, isPresent and negativeValuesAllowed (if they are provided as lists)."
    )
  }

  dimensionNames <- rep(ospDimensions$Amount, length(quantityPaths))
  icTask <- .getMoBiTaskFromCache("InitialConditionsTask")

  icTask$call(
    "SetInitialConditions",
    initialConditionsBuildingBlock,
    as.vector(quantityPaths, mode = "character"),
    as.vector(dimensionNames, mode = "character"),
    as.vector(quantityValues, mode = "numeric"),
    as.vector(scaleDivisors, mode = "numeric"),
    as.vector(isPresent, mode = "logical"),
    as.vector(negativeValuesAllowed, mode = "logical")
  )

  return(invisible(initialConditionsBuildingBlock))
}


# ' Set or add initial conditions to an existing Initial Conditions building block with values defined by formulas.
# '
# ' @param initialConditionsBuildingBlock A `BuildingBlock` object of type `Initial Conditions`.
# ' The entries will be added to or set in this building block.
# ' @param quantityPaths A list of full paths of the quantities (usually molecules). Should contain
# ' all path elements and the molecule name, separated by `|`.
# ' @param formulas A list of `Formula` objects that will be set for the quantities. The length of this list should be equal to the length of `quantityPaths`.
# ' @param scaleDivisors Either a single value or a list of scale divisors for the quantities. By default, the value is set to 1 for all quantities.
# ' If only single value is provided, the value will be set for all quantities.
# ' If a list is provided, the length of this list should be equal to the length of `quantityPaths`.
# ' @param isPresent Either a single value (`TRUE` or `FALSE`) or a list of boolean
# ' values indicating whether the quantity is present or not. If a list is provided,
# ' the length of this list should be equal to the length of `quantityPaths`.
# ' @param negativeValuesAllowed A single boolean value or a list of boolean values indicating whether negative values are
# ' allowed for the quantities. If a list is provided, the length of this list should be equal to the length of `quantityPaths`.
# '
# ' @returns The updated `initialConditionsBuildingBlock` object.
# ' TBD: no return? To be consistent (or not confusing) with the extend functions?
# ' TODO https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1815
# '
# ' @export
# ' @examples
# setInitialConditionsFormulas <- function(
#   initialConditionsBuildingBlock,
#   quantityPaths,
#   formulas,
#   scaleDivisors = 1,
#   isPresent = TRUE,
#   negativeValuesAllowed = FALSE
# ) {
#   return(initialConditionsBuildingBlock)
# }

#' Delete entries from an Initial Conditions Building Block
#'
#' @param initialConditionsBuildingBlock A `BuildingBlock` object of type `Initial Conditions`.
#' @param quantityPaths A list of full paths of the quantities (usually molecules) that
#' will be deleted. Entries not present in the provided BB are ignored. Should contain
#' all path elements and the molecule name, separated by `|`.
#'
#' @returns Invisibly returns the updated `initialConditionsBuildingBlock` object.
#' @export
#'
#' @examples
#' sim <- loadSimulation(system.file(
#'   "extdata",
#'   "simple.pkml",
#'   package = "ospsuite"
#' ))
#' module <- sim$configuration$modules[[1]]
#' icBB <- module$getInitialConditionsBBs()[[1]]
#' deleteInitialConditionsFromBB(
#'   icBB,
#'   quantityPaths = c("Organism|Liver|A", "Organism|Liver|B")
#' )
deleteInitialConditionsFromBB <- function(
  initialConditionsBuildingBlock,
  quantityPaths
) {
  .validateBuildingBlockType(
    initialConditionsBuildingBlock,
    BuildingBlockTypes$`Initial Conditions`
  )

  # Exit early if no quantity paths are provided
  if (length(quantityPaths) == 0) {
    return(invisible(initialConditionsBuildingBlock))
  }

  icTask <- .getMoBiTaskFromCache("InitialConditionsTask")
  icTask$call(
    "DeleteInitialConditions",
    initialConditionsBuildingBlock,
    as.vector(quantityPaths, mode = "character")
  )

  return(invisible(initialConditionsBuildingBlock))
}

#' Extend an Initial Conditions Building Block (BB) with new entries for molecules
#' from a molecules BB in all physical containers of a spatial structure BB.
#'
#' If an initial condition for the combination <container, molecule> already exists
#' in the IC-BB, the value will be kept AS IS.
#'
#' @param initialConditionsBuildingBlock A `BuildingBlock` object of type `Initial Conditions`.
#' @param spatialStructureModule A module with a spatial structure building block.
#' Entries will be created for the selected molecules in all physical containers of the
#' spatial structure.
#' @param moleculesModule A module with a molecules building block. The entries will be
#' created for all molecules from this building block, or for a subset of molecules
#' defined in the `moleculeNames` argument.
#' @param moleculeNames Optional list of molecule names. If provided, only the molecules
#' with these names will be added to the `initialConditionsBuildingBlock`. If a specified molecule is not present in the provided molecules BB, it will be ignored.
#'
#' @returns Paths of entries added to the building block.
#' @export
extendInitialConditionsBB <- function(
  initialConditionsBuildingBlock,
  spatialStructureModule,
  moleculesModule,
  moleculeNames = NULL
) {
  .validateBuildingBlockType(
    initialConditionsBuildingBlock,
    BuildingBlockTypes$`Initial Conditions`
  )
  # Get the spatial structure BB from the provided module
  spatialStructureBB <- .getBBFromModule(
    spatialStructureModule,
    bbType = "SpatialStructure"
  )
  # Get the molecules BB from the provided module
  moleculesBB <- .getBBFromModule(moleculesModule, bbType = "Molecules")
  # If molecule names are not provided, supply an empty list.
  if (is.null(moleculeNames)) {
    moleculeNames <- vector(mode = "character")
  }

  # Throw an error if any of the provided modules does not contain the required BB
  if (is.null(spatialStructureBB) || is.null(moleculesBB)) {
    missingBBs <- c(
      if (is.null(spatialStructureBB)) "Spatial Structure",
      if (is.null(moleculesBB)) "Molecules"
    )
    stop(
      paste0(
        "The provided modules do not contain the required building blocks: ",
        paste(missingBBs, collapse = ", "),
        ". "
      ),
      "Please provide modules with the required building blocks to be able to extend the initial conditions building block."
    )
  }

  # Get InitialConditionsTask
  icTask <- .getMoBiTaskFromCache("InitialConditionsTask")
  # Call the task method to extend the IC BB
  newPaths <- icTask$call(
    "ExtendInitialConditions",
    initialConditionsBuildingBlock,
    spatialStructureBB,
    moleculesBB,
    moleculeNames
  )

  return(newPaths)
}


#' Convert a Parameter Values Building Block to a data frame.
#'
#' @param parameterValuesBuildingBlock A `BuildingBlock` object of type `Parameter Values` or `Individual`.
#'
#' @returns A data frame with the following columns:
#' - `Container Path`: Full path to the container where the parameter is located.
#' - `Parameter Name`: Name of the parameter.
#' - `Value`: Value of the parameter. For values that are defined by a formula, the return value can be `NaN`.
#' - `Unit`: Unit of the parameter value.
#' - `Value Origin`: Origin of the parameter value.
#'
#' @export
#'
#' @examples
#' sim <- loadSimulation(system.file(
#'   "extdata",
#'   "simple.pkml",
#'   package = "ospsuite"
#' ))
#' module <- sim$configuration$modules[[1]]
#' pvBB <- module$getParameterValuesBBs()[[1]]
#' df <- parameterValuesBBToDataFrame(pvBB)
parameterValuesBBToDataFrame <- function(parameterValuesBuildingBlock) {
  .validateBuildingBlockType(
    parameterValuesBuildingBlock,
    c(
      BuildingBlockTypes$`Parameter Values`,
      BuildingBlockTypes$`Individual`,
      BuildingBlockTypes$`Expression Profile`
    )
  )

  taskName <- if (
    parameterValuesBuildingBlock$type == BuildingBlockTypes$`Parameter Values`
  ) {
    "ParameterValuesTask"
  } else if (
    parameterValuesBuildingBlock$type == BuildingBlockTypes$`Individual`
  ) {
    "IndividualTask"
  } else if (
    parameterValuesBuildingBlock$type == BuildingBlockTypes$`Expression Profile`
  ) {
    "ExpressionProfilesTask"
  }
  pvTask <- .getMoBiTaskFromCache(taskName)

  paths <- pvTask$call("AllPathsFrom", parameterValuesBuildingBlock)
  containerPaths <- vapply(
    paths,
    function(path) {
      .getParentContainerPath(path)
    },
    character(1),
    USE.NAMES = FALSE
  )
  parameterNames <- vapply(
    paths,
    function(path) {
      tail(toPathArray(path), 1)
    },
    character(1),
    USE.NAMES = FALSE
  )
  values <- pvTask$call("AllValuesFrom", parameterValuesBuildingBlock, paths)
  units <- pvTask$call("AllUnitsFrom", parameterValuesBuildingBlock, paths)
  valueOrigins <- pvTask$call(
    "AllValueOriginsFrom",
    parameterValuesBuildingBlock,
    paths
  )

  df <- data.frame(
    "Container Path" = containerPaths,
    "Parameter Name" = parameterNames,
    "Value" = values,
    "Unit" = units,
    "Value Origin" = valueOrigins,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  return(df)
}

#' Convert an Expression Profiles Building Block to data frames.
#' Convert an Expression Profiles Building Block to data frames.
#'
#' Returns both the expression parameter values and the initial conditions
#' contained in the expression profile building block.
#'
#' @param expressionProfilesBuildingBlock A `BuildingBlock` object of type `Expression Profile`.
#'
#' @returns A named list with two data frames:
#' - `expressionParameters`: A data frame with the following columns:
#'   - `Container Path`: Full path to the container where the parameter is located.
#'   - `Parameter Name`: Name of the parameter.
#'   - `Value`: Value of the parameter. For values that are defined by a formula, the return value can be `NaN`.
#'   - `Unit`: Unit of the parameter value.
#'   - `Value Origin`: Origin of the parameter value.
#' - `initialConditions`: A data frame with the following columns:
#'   - `Container Path`: Full path to the container where the molecule is located.
#'   - `Molecule Name`: Name of the molecule.
#'   - `Is Present`: Boolean indicating if the molecule is present.
#'   - `Value`: Initial value of the molecule. For values that are defined by a formula, the return value can be `NaN`.
#'   - `Unit`: Unit of the initial value.
#'   - `Scale Divisor`: Scale divisor for the initial value.
#'   - `Neg. Values Allowed`: Boolean indicating if negative values are allowed.
#'
#' @export
expressionProfileBBToDataFrame <- function(expressionProfilesBuildingBlock) {
  .validateBuildingBlockType(
    expressionProfilesBuildingBlock,
    BuildingBlockTypes$`Expression Profile`
  )

  epTask <- .getMoBiTaskFromCache("ExpressionProfileTask")

  paths <- epTask$call("AllPathsFrom", expressionProfilesBuildingBlock)
  containerPaths <- vapply(
    paths,
    function(path) {
      .getParentContainerPath(path)
    },
    character(1),
    USE.NAMES = FALSE
  )
  parameterNames <- vapply(
    paths,
    function(path) {
      tail(toPathArray(path), 1)
    },
    character(1),
    USE.NAMES = FALSE
  )
  values <- epTask$call("AllValuesFrom", expressionProfilesBuildingBlock, paths)
  units <- epTask$call("AllUnitsFrom", expressionProfilesBuildingBlock, paths)
  valueOrigins <- epTask$call(
    "AllValueOriginsFrom",
    expressionProfilesBuildingBlock,
    paths
  )

  expressionParametersDf <- data.frame(
    "Container Path" = containerPaths,
    "Parameter Name" = parameterNames,
    "Value" = values,
    "Unit" = units,
    "Value Origin" = valueOrigins,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  initialConditionsDf <- bbWithInitialConditionsToDataFrame(
    expressionProfilesBuildingBlock
  )

  return(list(
    expressionParameters = expressionParametersDf,
    initialConditions = initialConditionsDf
  ))
}

#' Set or add parameter values to an existing Parameter Values building block.
#'
#' This function allows adding or modifying parameter value entries. Only constant
#' values are allowed. Values are expected in the provided `units` and will be
#' converted to base units internally.
#'
#' @param parameterValuesBuildingBlock A `BuildingBlock` object of type `Parameter Values`.
#' The entries will be added to or set in this building block.
#' @param quantityPaths A list of full paths of the quantities (usually parameters). Should contain
#' all path elements and the parameter name, separated by `|`.
#' @param quantityValues A list of values for the quantities in the provided `units`.
#' The length of this list should be equal to the length of `quantityPaths`.
#' @param units A single unit string or a list of unit strings for the quantity values.
#' If a single string is provided, it will be used for all quantities.
#' @param dimensions A single dimension or a list of dimensions (string names)
#' of parameter values. Supported dimensions are listed in `ospDimensions`. If `NULL`
#' (default), dimensions are derived from the provided `units` using `getDimensionForUnit()`.
#'
#' @returns Invisibly returns the updated `parameterValuesBuildingBlock` object.
#' @export
#'
#' @examples
#' sim <- loadSimulation(system.file(
#'   "extdata",
#'   "simple.pkml",
#'   package = "ospsuite"
#' ))
#' module <- sim$configuration$modules[[1]]
#' pvBB <- module$getParameterValuesBBs()[[1]]
#' setParameterValuesInBB(
#'   pvBB,
#'   quantityPaths = "Organism|Organ|Parameter",
#'   quantityValues = 1.33,
#'   units = "ml/min/kg"
#' )
setParameterValuesInBB <- function(
  parameterValuesBuildingBlock,
  quantityPaths,
  quantityValues,
  units,
  dimensions = NULL
) {
  .validateBuildingBlockType(
    parameterValuesBuildingBlock,
    c(BuildingBlockTypes$`Parameter Values`)
  )
  validatedInputs <- .validatePVBBInputs(
    parameterValuesBuildingBlock,
    quantityPaths,
    quantityValues,
    units,
    dimensions
  )
  baseValues <- validatedInputs$baseValues
  dimensions <- validatedInputs$dimensions

  pvTask <- .getMoBiTaskFromCache("ParameterValuesTask")

  pvTask$call(
    "SetParameterValues",
    parameterValuesBuildingBlock,
    as.vector(quantityPaths, mode = "character"),
    as.vector(baseValues, mode = "numeric"),
    as.vector(dimensions, mode = "character")
  )

  return(invisible(parameterValuesBuildingBlock))
}


# #' Set or add parameter values to an existing Parameter Values building block with values defined by formulas.
# #'
# #' @param parameterValuesBuildingBlock A `BuildingBlock` object of type `Parameter Values`.
# #' The entries will be added to or set in this building block.
# #' @param quantityPaths A list of full paths of the quantities (usually parameters). Should contain
# #' all path elements and the parameter name, separated by `|`.
# #' @param formulas A list of `Formula` objects that will be set for the quantities. The length of this list should be equal to the length of `quantityPaths`.
# #' The dimension of the parameter will be set to the dimension of the formula.
# #' @param quantityValues
# #'
# #' @returns
# #'
# #' @export
# #' @examples
# setParameterValuesFormulas <- function(
#   parameterValuesBuildingBlock,
#   quantityPaths,
#   formulas
# ) {
#   return(parameterValuesBuildingBlock)
# }

#' Delete entries from a Parameter Values Building Block
#'
#' @param parameterValuesBuildingBlock A `BuildingBlock` object of type `Parameter Values`.
#' @param quantityPaths A list of full paths of the quantities (usually parameters)
#' that will be deleted. Should contain all path elements and the parameter name, separated by `|`.
#' Entries not present in the provided BB are ignored.
#'
#' @returns Invisibly returns the updated `parameterValuesBuildingBlock` object.
#' @export
#'
#' @examples
#' sim <- loadSimulation(system.file(
#'   "extdata",
#'   "simple.pkml",
#'   package = "ospsuite"
#' ))
#' module <- sim$configuration$modules[[1]]
#' pvBB <- module$getParameterValuesBBs()[[1]]
#' deleteParameterValuesFromBB(pvBB, quantityPaths = "Organism|Volume")
deleteParameterValuesFromBB <- function(
  parameterValuesBuildingBlock,
  quantityPaths
) {
  .validateBuildingBlockType(
    parameterValuesBuildingBlock,
    BuildingBlockTypes$`Parameter Values`
  )

  # Exit early if no quantity paths are provided
  if (length(quantityPaths) == 0) {
    return(invisible(parameterValuesBuildingBlock))
  }

  pvTask <- .getMoBiTaskFromCache("ParameterValuesTask")
  pvTask$call(
    "DeleteParameterValues",
    parameterValuesBuildingBlock,
    as.vector(quantityPaths, mode = "character")
  )

  return(invisible(parameterValuesBuildingBlock))
}

#' Extend a Parameter Values Building Block (BB) with local molecule parameters
#' for molecules from a molecules module in all physical containers of a spatial structure module.
#'
#' Existing entries will not be overwritten.
#'
#' @param parameterValuesBuildingBlock A `BuildingBlock` object of type `Parameter Values`.
#' @param spatialStructureModule A `MoBiModule` containing a `Spatial Structure` building block.
#' Entries will be created for local parameters of the selected molecules in all physical containers of this
#' spatial structure.
#' @param moleculesModule A `MoBiModule` containing a `Molecules` building block. The entries will be
#' created for all molecules from this building block, or for a subset of molecules
#' defined in the `moleculeNames` argument.
#' @param moleculeNames Optional list of molecule names. If provided, only the molecules
#' with these names will be added to the `parameterValuesBuildingBlock`. If a specified molecule is not present in the provided molecules BB, it will be ignored.
#'
#' @returns Path of entries added to the building block.
#' @export
addLocalMoleculeParametersToParameterValuesBB <- function(
  parameterValuesBuildingBlock,
  spatialStructureModule,
  moleculesModule,
  moleculeNames = NULL
) {
  .validateBuildingBlockType(
    parameterValuesBuildingBlock,
    BuildingBlockTypes$`Parameter Values`
  )
  # Get the spatial structure BB from the provided module
  spatialStructureBB <- .getBBFromModule(
    spatialStructureModule,
    bbType = "SpatialStructure"
  )
  # Get the molecules BB from the provided module
  moleculesBB <- .getBBFromModule(moleculesModule, bbType = "Molecules")
  # If molecule names are not provided, supply an empty list.
  if (is.null(moleculeNames)) {
    moleculeNames <- vector(mode = "character")
  }

  # Throw an error if any of the provided modules does not contain the required BB
  if (is.null(spatialStructureBB) || is.null(moleculesBB)) {
    missingBBs <- c(
      if (is.null(spatialStructureBB)) "Spatial Structure",
      if (is.null(moleculesBB)) "Molecules"
    )
    stop(
      paste0(
        "The provided modules do not contain the required building blocks: ",
        paste(missingBBs, collapse = ", "),
        ". "
      ),
      "Please provide modules with the required building blocks to be able to extend the parameter values building block."
    )
  }

  # Get ParameterValuesTask
  pvTask <- .getMoBiTaskFromCache("ParameterValuesTask")
  # Call the task method to add local molecule parameters to the PV BB
  newPaths <- pvTask$call(
    "AddLocalMoleculeParameters",
    parameterValuesBuildingBlock,
    spatialStructureBB,
    moleculesBB,
    moleculeNames
  )

  return(newPaths)
}

#' Extend a Parameter Values Building Block (BB) with protein expression parameters
#' for selected protein molecules in the selected organs.
#'
#' TBD: is a spatial structure required? Option 1 - define the organs as paths
#' Option 2 - provide a spatial structure and then define organ paths. Entries will
#' be created for all sub-organs.
#'
#' TBD: Is a molecules BB required? Or just molecule names?
#'
#' @param spatialStructureBB A `BuildingBlock` object of type `Spatial Structure`.
#' Entries will be created for the selected molecules in all physical containers of this
#' spatial structure.
#' @param moleculesBB A `BuildingBlock` object of type `Molecules`. The entries will be
#' created for all proteins from this building block, or for a subset of protein molecules
#' defined in the `moleculeNames` argument.
#' @param moleculeNames Optional list of protein molecule names. If provided, only the molecules
#' with these names will be added to the `parameterValuesBuildingBlock`.
#' @param parameterValuesBuildingBlock A `BuildingBlock` object of type `Parameter Values`.
#' @param organPaths A vector of paths to the organs for which the expression paramters
#' will be created. If any of the provided path is not an organ, an error is thrown.
#' If `NULL` (default), the function will use all organs from the spatial structure.
#'
#' @returns Path of entries added to the building block.
#' @export
addProteinExpressionToParameterValuesBB <- function(
  parameterValuesBuildingBlock,
  spatialStructureBB,
  organPaths = NULL,
  moleculesBB,
  moleculeNames = NULL
) {}


#' Validate Building Block Type
#'
#' Internal utility function to validate the type of a building block.
#'
#' @param buildingBlock A `BuildingBlock` object to validate.
#' @param expectedTypes A character vector indicating the expected types of the building block.
#'
#' @keywords internal
#' @noRd
.validateBuildingBlockType <- function(
  buildingBlock,
  expectedTypes
) {
  validateIsOfType(buildingBlock, "BuildingBlock")
  if (is.null(buildingBlock)) {
    return(invisible(TRUE))
  }

  if (!buildingBlock$type %in% expectedTypes) {
    # throw error
    stop(messages$errorWrongBuildingBlockType(
      buildingBlock$name,
      expectedTypes,
      buildingBlock$type
    ))
  }
  return(invisible(TRUE))
}

#' Validate and process Parameter Values Building Block Inputs.
#'
#' Internal utility function to validate the inputs for setting parameter values in Building Blocks that have a "Parameter Values" structure. These include
#' the Parameter Values BB, the Individual, and the Expression Profiles BBs.
#' Following validation and conversion is performed:
#' - If `units` is provided as a single string, it is replicated to match the length of `quantityPaths`.
#' - If `dimensions` is provided as a single string, it is replicated to match the length of `quantityPaths`.
#' - It is validated that all provided units are supported in the OSPSuite platform. If any unsupported unit is found, an error is thrown listing the unsupported units.
#' - It is validated that the lengths of `quantityPaths`, `quantityValues`, `units`, and `dimensions` (if provided as lists) are all equal. If not, an error is thrown indicating the mismatch.
#' - If `dimensions` is not provided, it is derived from the provided `units` using the `getDimensionForUnit()` function.
#' - The `quantityValues` are converted from the provided `units` to base units using the `toBaseUnit()` function, and the converted values are returned in a list along with the dimensions.
#'
#' @param parameterValuesBuildingBlock A `BuildingBlock` object of type `Parameter Values` or `Individual` or `Expression Profiles`.
#' @param quantityPaths A character vector of quantity paths to set.
#' @param quantityValues A numeric vector of values to assign. Must have the same length as `quantityPaths`.
#' @param units A single unit string or a list of unit strings for the quantity values.
#' @param dimensions A single dimension string or a list of dimension strings for the quantity values.
#'
#' @returns A list containing the converted `baseValues` and their corresponding `dimensions`.
#' @keywords internal
#'
#' @noRd
.validatePVBBInputs <- function(
  parameterValuesBuildingBlock,
  quantityPaths,
  quantityValues,
  units,
  dimensions
) {
  # Exit early if no quantity paths are provided
  if (length(quantityPaths) == 0) {
    return(list(
      baseValues = numeric(0),
      dimensions = character(0)
    ))
  }

  # Replicate scalar units and dimensions to match the length of quantityPaths
  if (length(units) == 1) {
    units <- rep(units, length(quantityPaths))
  }

  # Validate if any unit is not supported
  if (!all(sapply(units, isSupportedUnit))) {
    unsupportedUnits <- units[!sapply(units, isSupportedUnit)]
    stop(
      paste0(
        "The following units are not supported in the OSPSuite platform: ",
        paste(unique(unsupportedUnits), collapse = ", "),
        ". Please provide supported units to set parameter values in the building block."
      )
    )
  }

  if (!is.null(dimensions) && length(dimensions) == 1) {
    dimensions <- rep(dimensions, length(quantityPaths))
  }

  if (
    length(quantityPaths) != length(quantityValues) ||
      length(quantityPaths) != length(units) ||
      (!is.null(dimensions) && length(quantityPaths) != length(dimensions))
  ) {
    stop(
      "The length of quantityPaths should be equal to the length of quantityValues, units, and dimensions (if provided as lists)."
    )
  }

  # Derive dimensions from units if not provided
  if (is.null(dimensions)) {
    dimensions <- vapply(
      units,
      getDimensionForUnit,
      character(1),
      USE.NAMES = FALSE
    )
  }

  # Convert values from provided units to base units
  baseValues <- vapply(
    seq_along(quantityValues),
    function(i) {
      toBaseUnit(dimensions[i], quantityValues[i], units[i])
    },
    numeric(1),
    USE.NAMES = FALSE
  )

  return(list(
    baseValues = baseValues,
    dimensions = dimensions
  ))
}
