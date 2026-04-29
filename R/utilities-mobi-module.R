#' Load a MoBi module from pkml.
#'
#' @param path Path to the pkml file with a module export
#'
#' @returns A `MoBiModule` object
#' @export
#'
#' @examples
#' filePath <- system.file("extdata", "Thyroid.pkml", package = "ospsuite")
#' module <- loadModuleFromPKML(filePath)
loadModuleFromPKML <- function(path) {
  if (!file.exists(path)) {
    stop(paste0("File does not exist: ", path))
  }
  validateIsFileExtension(path, "pkml")
  netObject <- .callModuleTask("LoadModulesFromFile", path.expand(path))
  if (length(netObject) > 1) {
    stop(
      "The PKML you are trying to load the module from contains more than one module, but the 
    function expects only one module.
    Most probably you are trying to load a simulation export."
    )
  }
  # .NET always returns an array, as it also returns multiple modules for a
  # simulation export. In R, this function should only be used for loading single modules.
  module <- MoBiModule$new(netObject[[1]])

  return(module)
}

#' Create a ModuleConfiguration .net object
#'
#' @param module A `Module` object representing the module to configure.
#' @param selectedParameterValueName Name of a Parameter Values Building Block contained in this module that will be selected in the Model Configuration.
#' If `NULL`, no PV BB will be selected.
#' @param selectedInitialConditionName Name of an Initial Conditions Building Block contained in this module that will be selected in the Model Configuration.
#' If `NULL`, no IC BB will be selected.
#'
#' @returns A `ModuleConfiguration` object representing the configuration of the module.
#' @noRd
.createModuleConfiguration <- function(
  module,
  selectedParameterValueName = NULL,
  selectedInitialConditionName = NULL
) {
  # If selected PV or IC BB names are NULL, no BB will be selected in the configuration
  # Passing a NULL to .NET does not work, so we have to substitute with empty strings
  # This works because on .NET side, the check is for null or empty
  selectedParameterValueName <- selectedParameterValueName %||% ""
  selectedInitialConditionName <- selectedInitialConditionName %||% ""

  netTask <- .getMoBiTaskFromCache("SimulationTask")
  netModuleConfiguration <- netTask$call(
    "CreateModuleConfiguration",
    module,
    selectedParameterValueName,
    selectedInitialConditionName
  )

  return(netModuleConfiguration)
}

#' Call a method of a MoBi.CLI.Core.Services.ModuleTask
#'
#' @param property The name of the property or method to call on the `ModuleTask`.
#' @param ... Additional arguments to pass to the method.
#' @returns The result of the method call.
#' @noRd
.callModuleTask <- function(property, ...) {
  netTask <- .getMoBiTaskFromCache("ModuleTask")
  results <- netTask$call(property, ...)
  return(results)
}

#' Call a method of a `ModuleTask` that takes a variable number of building
#' blocks (a `params IBuildingBlock[]` argument on the engine side).
#'
#' Spreads `buildingBlocks` as individual positional arguments so that each is
#' marshalled as an element of the `params` array. `toList(NULL)` would yield
#' `list(NULL)`, so a `NULL` `buildingBlocks` is guarded explicitly.
#'
#' @param method Name of the method to call on the `ModuleTask`.
#' @param ... Leading positional arguments (e.g. module name or module).
#' @param buildingBlocks A `BuildingBlock`, a list of `BuildingBlock` objects,
#'   or `NULL`.
#' @returns The result of the method call.
#' @noRd
.callModuleTaskWithBBs <- function(method, ..., buildingBlocks) {
  netTask <- .getMoBiTaskFromCache("ModuleTask")
  bbList <- if (is.null(buildingBlocks)) {
    list()
  } else {
    ospsuite.utils::toList(buildingBlocks)
  }
  args <- c(list(method, ...), bbList)
  do.call(netTask$call, args)
}

#' Create a new MoBi module
#'
#' Creates an empty MoBi module, optionally pre-populated with a list of
#' building blocks.
#'
#' @param name Name of the module to create. Must be a non-empty string.
#' @param buildingBlocks Optional `BuildingBlock`, list of `BuildingBlock`
#'   objects, or `NULL` (default) for an empty module.
#'
#' @returns A `MoBiModule` object.
#'
#' @details Single-type building blocks (Molecules, Reactions, Spatial
#'   Structure, Passive Transports, Observers, Event Groups) can appear at
#'   most once per module; passing two BBs of the same single type raises an
#'   error. Initial Conditions and Parameter Values BBs may appear multiple
#'   times.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Empty module
#' module <- createMoBiModule("MyModule")
#'
#' # Module pre-populated with a Parameter Values BB loaded from a pkml file
#' pvBB <- loadBuildingBlockFromPKML(
#'   filePath = "ParameterValues.pkml",
#'   type = BuildingBlockTypes$`Parameter Values`
#' )
#' module <- createMoBiModule("MyModule", buildingBlocks = pvBB)
#' }
createMoBiModule <- function(name, buildingBlocks = NULL) {
  validateIsString(name)
  ospsuite.utils::validateHasOnlyNonEmptyStrings(name)
  validateIsOfType(buildingBlocks, "BuildingBlock", nullAllowed = TRUE)

  netObject <- .callModuleTaskWithBBs(
    "CreateModule",
    name,
    buildingBlocks = buildingBlocks
  )
  return(MoBiModule$new(netObject))
}

#' Get a list of Parameter Values (PV) or Initial Conditions (IC) Building Blocks (BBs) in the module.
#'
#' @param module The `MoBiModule` object for which to retrieve the Building Blocks.
#' @param names Optional names of the Parameter Values (PV) or the Initial Conditions (IC) Building Block to retrieve.
#' If `NULL`, returns all PV/IC BBs.
#' @param bbType Type of Building Block to retrieve, either "Parameter Values" or "Initial Conditions".
#' @param stopIfNotFound If `TRUE` (default), an error is thrown if any of the specified
#' BB is not present in the project.
#' @returns A named list of `BuildingBlock` objects, with names being the names of the PV or IC BBs.
#' @noRd
.getICPVBBsFromModule <- function(
  module,
  names = NULL,
  bbType,
  stopIfNotFound
) {
  if (bbType == BuildingBlockTypes$`Parameter Values`) {
    allNames <- module$parameterValuesBBnames
    allMethodName <- "AllParameterValuesBuildingBlocksFromModule"
    byNameMethodName <- "ParameterValuesBuildingBlockByName"
  } else if (bbType == BuildingBlockTypes$`Initial Conditions`) {
    allNames <- module$initialConditionsBBnames
    allMethodName <- "AllInitialConditionsBuildingBlocksFromModule"
    byNameMethodName <- "InitialConditionBuildingBlockByName"
  } else {
    stop(
      paste0(
        "Invalid Building Block type. Must be one of the following: ",
        paste(
          c(
            BuildingBlockTypes$`Parameter Values`,
            BuildingBlockTypes$`Initial Conditions`
          ),
          collapse = ", "
        )
      )
    )
  }

  # Check if any of the provided names are not present in the module
  missingNames <- setdiff(names, allNames)
  if (length(missingNames) > 0 && stopIfNotFound) {
    stop(paste(
      "No",
      bbType,
      "Building Blocks found with names:",
      paste(missingNames, collapse = ", "),
      "in module",
      module$name
    ))
  }

  # If stopIfNotFound is FALSE, filter only the names that are present in the project
  if (is.null(names)) {
    names <- allNames
    # If no names are provided, just return all available BBs of this type
    bbsNet <- .callModuleTask(allMethodName, module)
  } else {
    names <- intersect(names, allNames)
    bbsNet <- lapply(names, function(name) {
      .callModuleTask(byNameMethodName, module, name)
    })
  }

  # Create BuildingBlock objects
  bbs <- lapply(bbsNet, function(bb) {
    BuildingBlock$new(bb, type = bbType)
  })
  names(bbs) <- names

  return(bbs)
}


#' Get a specified Building Block (BBs) from the module.
#' To get IC or PV BBs, use the `.getICPVBBsFromModule` function.
#'
#' @param module The `MoBiModule` object for which to retrieve the Building Block.
#' @param bbType Type of Building Block to retrieve. One of the following: "SpatialStructure", "Molecules", "Reactions", "Passive Transports", "Observers", "EventGroups".
#' @returns A `BuildingBlock` object, or `NULL` if no BB of the specified type is present in the module.
#' @noRd
.getBBFromModule = function(
  module,
  bbType
) {
  if (!(bbType %in% names(BuildingBlockTypes))) {
    stop(
      paste0(
        "Invalid Building Block type. Must be one of the following: ",
        paste(names(BuildingBlockTypes), collapse = ", ")
      )
    )
  }
  bbNet <- module$get(bbType)
  if (is.null(bbNet)) {
    return(NULL)
  }

  # Molecules BBs use a dedicated subclass exposing molecule-name queries.
  if (bbType == BuildingBlockTypes$Molecules) {
    return(MoleculesBuildingBlock$new(bbNet))
  }

  # Create BuildingBlock object
  return(BuildingBlock$new(bbNet, type = bbType))
}
