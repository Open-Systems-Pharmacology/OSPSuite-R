#' Set or add initial conditions to an existing Initial Conditions building block.
#'
#' This functions allows adding or modifying initial condition entries.
#'
#' @param initialConditionsBuildingBlock A `BuildingBlock` object of type `Initial Conditions`.
#' The entries will be added to or set in this building block.
#' @param quantityPaths A list of full paths of the quantities (usually molecules). Should contain
#' all path elements and the molecule name, separated by `|`.
#' @param quantityValues A list of values for the quantities. Should be `NULL` if
#' the argument `formulas` is provided. If not `NULL`, the length of this list should be equal to the length of `quantityPaths`.
#' @param scaleDivisors Either a single value or a list of scale divisors for the quantities.
#' If only single value is provided, the value will be set for all quantities.
#' If a list is provided, the length of this list should be equal to the length of `quantityPaths`.
#' @param isPresent Either a single value (`TRUE` or `FALSE`) or a list of boolean
#' values indicating whether the quantity is present or not. If a list is provided,
#' the length of this list should be equal to the length of `quantityPaths`.
#' @param negativeValuesAllowed A single boolean value or a list of boolean values
#' indicating whether negative values are allowed for the quantities. If a list is provided,
#' the length of this list should be equal to the length of `quantityPaths`.
#'
#' @param formulas A list of `Formula` objects that will be set for the quantities.
#' Mixing of `formulas` and `quantityValues` is not allowed. If `formulas` is provided,
#' `quantityValues` should be `NULL`. If not `NULL`, the length of this list should be equal to the length of `quantityPaths`.
#'
#' @returns The updated `initialConditionsBuildingBlock` object.
#' TBD: no return? To be consistent (or not confusing) with the extend functions?
#' @export
#'
#' @examples
setInitialConditions <- function(initialConditionsBuildingBlock,
                                 quantityPaths,
                                 quantityValues,
                                 scaleDivisors = 1,
                                 isPresent = TRUE,
                                 negativeValuesAllowed = FALSE,
                                 formulas = NULL) {
  return(initialConditionsBuildingBlock)
}

#' Delete entries from an Initial Conditions Building Block
#'
#' @param initialConditionsBuildingBlock A `BuildingBlock` object of type `Initial Conditions`.
#' @param quantityPaths A list of full paths of the quantities (usually molecules) that
#' will be deleted. Entries not present in the provided BB are ignored. Should contain
#' all path elements and the molecule name, separated by `|`.
#'
#' @export
#'
#' @examples
deleteInitialConditions <- function(initialConditionsBuildingBlock,
                                    quantityPaths) {
}

#' Extend an Initial Conditions Building Block (BB) with new entries for molecules
#' from a molecules BB in all physical containers of a spatial structure BB.
#'
#' If an initial condition for the combination {container, molecule} already exists
#' in the IC-BB, the value will be kept AS IS.
#'
#' @param initialConditionsBuildingBlock A `BuildingBlock` object of type `Initial Conditions`.
#' @param spatialStructureBB A `BuildingBlock` object of type `Spatial Structure`.
#' Entries will be created for the selected molecules in all physical containers of this
#' spatial structure.
#' @param moleculesBB A `BuildingBlock` object of type `Molecules`. The entries will be
#' created for all molecules from this building block, or for a subset of molecules
#' defined in the `moleculeNames` argument.
#' @param moleculeNames Optional list of molecule names. If provided, only the molecules
#' with these names will be added to the `initialConditionsBuildingBlock`.
#'
#' @returns Paths of entries added to the building block.
#' @export
#'
#' @examples
extendInitialConditions <- function(initialConditionsBuildingBlock,
                                    spatialStructureBB,
                                    moleculesBB,
                                    moleculeNames = NULL) {
}




#' Set or add parameter values to an existing Parameter Values building block.
#'
#' This functions allows adding or modifying parameter values entries.
#'
#' @param parameterValuesBuildingBlock A `BuildingBlock` object of type `Parameter Values`.
#' The entries will be added to or set in this building block.
#' @param quantityPaths A list of full paths of the quantities (usually parameters). Should contain
#' all path elements and the parameter name, separated by `|`.
#' @param formulas A list of `Formula` objects that will be set for the quantities.
#' Mixing of `formulas` and `quantityValues` is not allowed. If `formulas` is provided,
#' `quantityValues` should be `NULL`. The length of this list should be equal to the length of `quantityPaths`.
#' @param dimensions A single dimension or a list of dimensions (string names)
#' of parameter values. Supported dimensions are listed in `ospDimension`. By default,
#' new entries get the `Dimensionless` dimension.
#' @param quantityValues A list of values for the quantities. Should be `NULL` if
#' the argument `formulas` is provided. If not `NULL`, the length of this list should be equal to the length of `quantityPaths`.
#'
#' @export
#'
#' @examples
setParameterValues <- function(parameterValuesBuildingBlock,
                               quantityPaths,
                               quantityValues,
                               dimensions = ospDimensions$Dimensionless,
                               formulas = NULL) {
  return(parameterValuesBuildingBlock)
}

#' Delete entries from a Parameter Values Building Block
#'
#' @param parameterValuesBuildingBlock A `BuildingBlock` object of type `Parameter Values`.
#' @param quantityPaths A list of full paths of the quantities (usually parameters)
#' that will be deleted. Should contain all path elements and the parameter name, separated by `|`.
#' Entries not present in the provided BB are ignored.
#'
#' @export
#'
#' @examples
deleteParameterValues <- function(parameterValuesBuildingBlock,
                                  quantityPaths) {
}

#' Extend a Parameter Values Building Block (BB) with local molecule parameters
#' for molecules from a molecules BB in all physical containers of a spatial structure BB.
#'
#' Existing entries will not be overwritten.
#'
#' @param parameterValuesBuildingBlock A `BuildingBlock` object of type `Parameter Values`.
#' @param spatialStructureBB A `BuildingBlock` object of type `Spatial Structure`.
#' Entries will be created for local parameters of the selected molecules in all physical containers of this
#' spatial structure.
#' @param moleculesBB A `BuildingBlock` object of type `Molecules`. The entries will be
#' created for all molecules from this building block, or for a subset of molecules
#' defined in the `moleculeNames` argument.
#' @param moleculeNames Optional list of molecule names. If provided, only the molecules
#' with these names will be added to the `parameterValuesBuildingBlock`.
#'
#' @returns Path of entries added to the building block.
#' @export
#'
#' @examples
addLocalMoleculeParameters <- function(parameterValuesBuildingBlock,
                                       spatialStructureBB,
                                       moleculesBB,
                                       moleculeNames = NULL) {
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
#'
#' @examples
addProteinExpressionToParameterValuesBB <- function(parameterValuesBuildingBlock,
                                                    spatialStructureBB,
                                                    organPaths = NULL,
                                                    moleculesBB,
                                                    moleculeNames = NULL) {
}
