#' Retrieve all quantities of a container (simulation or container instance)
#' matching the given path criteria
#'
#' @param paths A vector of strings relative to the `container`
#' @param container A Container or Simulation used to find the parameters
#' @seealso [loadSimulation()], [getContainer()] and
#'   [getAllContainersMatching()] to retrieve objects of type Container or
#'   Simulation
#'
#' @return
#'
#' A list of quantities matching the path criteria. The list is empty if no
#' quantity matching were found.
#'
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Return all `Volume` quantities defined in all direct containers of the organism
#' quantities <- getAllQuantitiesMatching("Organism|*|Volume", sim)
#'
#' # Return all `Volume` quantities defined in all direct containers of the organism
#' # and the parameter 'Weight (tissue)' of the container 'Liver'
#' paths <- c("Organism|*|Volume", "Organism|Liver|Weight (tissue)")
#' quantities <- getAllQuantitiesMatching(paths, sim)
#'
#' # Returns all `Volume` quantities defined in `Organism` and all its subcontainers
#' quantities <- getAllQuantitiesMatching("Organism|**|Volume", sim)
#' @export
getAllQuantitiesMatching <- function(paths, container) {
  .getAllEntitiesMatching(paths, container, Quantity)
}

#' Retrieves the path of all quantities defined in the container and all its children
#'
#' @param container A Container or Simulation used to find the parameters
#' @seealso [loadSimulation()], [getContainer()] and
#'   [getAllContainersMatching()] to retrieve objects of type Container or
#'   Simulation
#'
#' @return An array with one entry per quantity defined in the container
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Returns the path of all quantities defined in the simulation
#' quantityPaths <- getAllQuantityPathsIn(sim)
#' @export
getAllQuantityPathsIn <- function(container) {
  .getAllEntityPathsIn(container, Quantity)
}

#' Retrieve a single quantity by path in the given container
#'
#' @inherit getAllQuantitiesMatching
#' @param path A string representing the path relative to the `container`
#' @param stopIfNotFound Boolean. If `TRUE` (default) and no quantity exists for
#'   the given path, an error is thrown. If `FALSE`, `NULL` is returned.
#'
#' @return The `Quantity` with the given path. If the quantity for the path
#' does not exist, an error is thrown if `stopIfNotFound` is `TRUE` (default),
#' otherwise `NULL`
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' quantity <- getQuantity("Organism|Liver|Volume", sim)
#' @export
getQuantity <- function(path, container, stopIfNotFound = TRUE) {
  .getEntity(path, container, Quantity, stopIfNotFound)
}


#' Set values of quantity
#'
#' @param quantities A single or a list of `Quantity`
#' @param values A numeric value that should be assigned to the quantity or a
#'   vector of numeric values, if the value of more than one quantity should be
#'   changed. Must have the same length as 'quantities'. Alternatively, the
#'   value can be a unique number. In that case, the same value will be set in
#'   all parameters
#' @param units A string or a list of strings defining the units of the
#'   `values`. If `NULL` (default), values are assumed to be in base units. If
#'   not `NULL`, must have the same length as `quantities`.
#'
.setQuantityValues <- function(quantities, values, units = NULL) {
  # Must turn the input into a list so we can iterate through even when only
  # one parameter is passed
  quantities <- toList(quantities)
  values <- c(values)

  # Test for correct inputs
  validateIsOfType(quantities, "Quantity")
  validateIsNumeric(values)

  if (length(values) > 1) {
    validateIsSameLength(quantities, values)
  } else {
    values <- rep(values, length(quantities))
  }

  if (!is.null(units)) {
    validateIsSameLength(quantities, units)
    validateIsString(units)
  }

  for (i in seq_along(quantities)) {
    quantity <- quantities[[i]]
    value <- values[[i]]
    if (!is.null(units)) {
      value <- toBaseUnit(quantityOrDimension = quantity, values = value, unit = units[[i]])
    }
    quantity$value <- value
  }
}

#' Set the values of quantities in the simulation by path
#'
#' @param quantityPaths A single or a list of absolute quantity paths
#' @param values A numeric value that should be assigned to the quantities or a
#'   vector of numeric values, if the value of more than one quantity should be
#'   changed. Must have the same length as 'quantityPaths'.
#' @param simulation Simulation containing the quantities
#' @param stopIfNotFound Boolean. If `TRUE` (default) and no quantity exists for
#'   the given path, an error is thrown. If `FALSE`, a warning is shown to the
#'   user.
#' @param units A string or a list of strings defining the units of the
#'   `values`. If `NULL` (default), values are assumed to be in base units. If
#'   not `NULL`, must have the same length as `quantityPaths`.
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' setQuantityValuesByPath("Organism|Liver|Volume", 1, sim)
#'
#' setParameterValuesByPath(list("Organism|Liver|Volume", "Organism|Liver|A"), c(2, 3), sim)
#' @export
setQuantityValuesByPath <- function(quantityPaths, values, simulation, units = NULL, stopIfNotFound = TRUE) {
  validateIsString(quantityPaths)
  validateIsNumeric(values)
  validateIsSameLength(quantityPaths, values)
  validateIsOfType(simulation, "Simulation")

  if (!is.null(units)) {
    validateIsSameLength(quantityPaths, units)
    validateIsString(units)
  }

  task <- .getNetTaskFromCache("ContainerTask")
  for (i in seq_along(quantityPaths)) {
    path <- quantityPaths[[i]]
    value <- values[[i]]
    if (!is.null(units)) {
      dimension <- task$call("DimensionNameByPath", simulation, path, stopIfNotFound)
      # Dimension ca be be empty if the path was not found
      if (dimension == "") {
        next
      }
      # If the unit is NULL, the value is assumend to be in base unit and no conversion
      # in necessary
      if (!is.null(units[[i]])) {
        mw <- simulation$molWeightFor(path)
        # Try to convert to base unit. If the provided unit is not valid for the
        # quantity dimension, throw an error
        tryCatch(
          {
            value <- toBaseUnit(
              quantityOrDimension = dimension,
              values = value,
              unit = units[[i]],
              molWeight = mw
            )
          },
          error = function(e) {
            stop(
              messages$wrongUnitForQuantity(
                quantityPath = path,
                unit = units[[i]],
                dimension = dimension
              )
            )
          }
        )
      }
    }

    task$call(
      "SetValueByPath",
      simulation,
      path,
      value,
      stopIfNotFound
    )
  }
}

#' Get the values of quantities in the simulation by path
#'
#' @param quantityPaths A single or a list of absolute quantity paths
#' @param simulation Simulation containing the quantities
#' @param stopIfNotFound Boolean. If `TRUE` (default) and no quantity exists for
#'   the given path, an error is thrown. If `FALSE`, a warning is shown to the
#'   user.
#' @param units A string or a list of strings defining the units of returned
#' values. If `NULL` (default), values are returned in base units. If not
#' `NULL`, must have the same length as `quantityPaths`. Single entries may be
#' `NULL`.
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' getQuantityValuesByPath(
#'   list("Organism|Liver|Volume", "Organism|Liver|A"),
#'   sim, list("ml", NULL)
#' )
#' @export
getQuantityValuesByPath <- function(quantityPaths, simulation, units = NULL, stopIfNotFound = TRUE) {
  validateIsString(quantityPaths)
  validateIsOfType(simulation, "Simulation")

  if (!is.null(units)) {
    validateIsSameLength(quantityPaths, units)
    validateIsString(units, nullAllowed = TRUE)
  }

  task <- .getNetTaskFromCache("ContainerTask")
  outputValues <- vector("numeric", length(quantityPaths))
  for (i in seq_along(quantityPaths)) {
    path <- quantityPaths[[i]]
    value <- task$call("GetValueByPath", simulation, path, stopIfNotFound)
    if (!is.null(units)) {
      dimension <- task$call(
        "DimensionNameByPath",
        simulation,
        path,
        stopIfNotFound
      )
      # Dimension ca be be empty if the path was not found
      if (dimension == "") {
        next
      }
      # If the unit is NULL, the value is assumend to be in base unit and no conversion
      # in necessary
      if (!is.null(units[[i]])) {
        mw <- simulation$molWeightFor(path)
        value <- toUnit(
          quantityOrDimension = dimension,
          values = value,
          targetUnit = units[[i]],
          molWeight = mw
        )
      }
    }

    outputValues[[i]] <- value
  }

  return(outputValues)
}

#' Scale current values of quantities using a factor
#'
#' @param quantities A single or a list of `Quantity`
#'
#' @param factor A numeric value that will be used to scale all quantities
#'
.scaleQuantityValues <- function(quantities, factor) {
  quantities <- c(quantities)

  # Test for correct inputs
  validateIsOfType(quantities, "Quantity")
  validateIsNumeric(factor)

  lapply(quantities, function(q) q$value <- q$value * factor)
  invisible()
}


#' Retrieves the display path of the quantity defined by path in the simulation
#'
#' @param paths A single string or array of paths path relative to the `Simulation`
#' @param simulation A `Simulation` used to find the entities
#'
#' @return a display path for each entry in paths
#'
.getQuantityDisplayPaths <- function(paths, simulation) {
  validateIsString(paths)
  validateIsOfType(simulation, "Simulation")
  displayResolver <- .getNetTask("FullPathDisplayResolver")
  paths <- c(paths)

  displayPaths <- lapply(paths, function(path) {
    quantity <- getQuantity(path, simulation, stopIfNotFound = FALSE)
    if (is.null(quantity)) {
      return(path)
    }

    return(displayResolver$call("FullPathFor", quantity))
  })

  return(unlist(displayPaths, use.names = FALSE))
}


#' Retrieves the path of all observers defined in the container and all its children
#'
#' @param container A Container or Simulation used to find the observers
#' @seealso [loadSimulation()], [getContainer()] and [getAllContainersMatching()] to retrieve objects of type Container or Simulation
#'
#' @return An array with one entry per observer defined in the container
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Returns the path of all quantities defined in the simulation
#' observerPaths <- getAllObserverPathsIn(sim)
#' @export
getAllObserverPathsIn <- function(container) {
  return(setdiff(
    x = getAllQuantityPathsIn(container),
    y = c(getAllParameterPathsIn(container), getAllMoleculePathsIn(container))
  ))
}

#' Is the value defined by an explicit formula
#'
#' @param path Path to the quantity
#' @param simulation A `Simulation` object that contains the quantity
#' @param stopIfNotFound Boolean. If `TRUE` (default) and no quantity exists
#' for the given path, an error is thrown. If `FALSE`, `FALSE` is returned.
#'
#' @return `TRUE` if the value is an explicit formula, `FALSE` otherwise.
#' Also returns `FALSE` if no quantity with the given path is found and
#' `stopInfNotFound` is set to `FALSE`.
#' @export
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' isExplicitFormulaByPath("Organism|Liver|Volume", sim) # FALSE
isExplicitFormulaByPath <- function(path, simulation, stopIfNotFound = TRUE) {
  validateIsString(path, nullAllowed = FALSE)
  validateIsOfType(simulation, "Simulation")

  task <- .getNetTaskFromCache("ContainerTask")
  # Check if the quantity is defined by an explicit formula
  isFormulaExplicit <- task$call("IsExplicitFormulaByPath", simulation, path, stopIfNotFound)

  return(isFormulaExplicit)
}

#' Retrieve molecular weight for a quantity's molecule
#'
#' @description Returns the molecular weight of the molecule for a given quantity.
#' If no unit is provided, the value is returned in the base unit (`kg/µmol`).
#'
#' @param quantity A `Quantity` object.
#' @param unit Optional. Target unit for the molecular weight. Defaults to `kg/µmol`.
#' @param stopIfNotFound Logical. If `TRUE`, throws an error when the molecular
#' weight cannot be retrieved. If `FALSE`, returns `NA`. Default is `FALSE`.
#' @return The molecular weight in the specified unit or `NA` if not found.
#' @export
#'
#' @examples
#' simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' parameterPath <- "Organism|VenousBlood|Plasma|Aciclovir|Concentration"
#' quantity <- getQuantity(parameterPath, container = sim)
#' getMolWeightFor(quantity, unit = "g/mol")
getMolWeightFor <- function(quantity, unit = NULL, stopIfNotFound = FALSE) {
  ospsuite.utils::validateIsOfType(quantity, "Quantity")
  ospsuite.utils::validateIsCharacter(unit, nullAllowed = TRUE)

  unit <- unit %||% getBaseUnit(ospDimensions$`Molecular weight`)

  isMolecule <- try(
    quantity$call("get_ContainerTypeAsString") == "Molecule",
    silent = TRUE
  )

  if (inherits(isMolecule, "try-error") || !isMolecule) {
    moleculeContainer <- .getParentContainerByType(quantity, "Molecule")
  } else {
    moleculeContainer <- quantity
  }

  if (!ospsuite.utils::isOfType(moleculeContainer, "Entity")) {
    if (stopIfNotFound) {
      stop(messages$molWeightErrorMessage(quantity$path))
    }
    return(NA)
  }

  paramPath <- paste(moleculeContainer$name, "Molecular weight", sep = "|")

  rootContainer <- .getParentContainerByType(quantity, "Simulation")
  task <- .getNetTask("ContainerTask")
  paramMW <- task$call("AllParametersMatching", rootContainer, paramPath)

  if (length(paramMW) == 0) {
    if (stopIfNotFound) {
      stop(messages$molWeightErrorMessage(quantity$path))
    }
    return(NA)
  }

  molWeight <- paramMW[[1]]$call("get_Value")
  molWeightConverted <- toUnit(ospDimensions$`Molecular weight`,
    values = molWeight,
    targetUnit = unit,
    sourceUnit = "kg/µmol"
  )

  return(molWeightConverted)
}

#' Get the parent container of a specific type for a given Entity
#'
#' @description Recursively retrieves the parent container of the specified type
#' for a given `Entity`.
#'
#' @param entity An `Entity` object or an object inheriting from `Entity`
#' @param type A string representing the container type to find (e.g.,
#' "Simulation", "Molecule").
#' @return The closest parent container of the specified type or `NA` if not found.
#' @keywords internal
.getParentContainerByType <- function(entity, type) {
  if (is.null(entity)) {
    return(NA)
  }
  ospsuite.utils::validateIsOfType(entity, "Entity")
  ospsuite.utils::validateIsCharacter(type)

  if (ospsuite.utils::isOfType(entity, "Container")) {
    if (entity$containerType == type) {
      return(entity)
    }
  }

  return(.getParentContainerByType(entity$parentContainer, type))
}
