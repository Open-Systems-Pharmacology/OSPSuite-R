#' Set or add initial conditions to an existing Intitial Conditions building block.
#'
#' This functions allows adding or modifying initial condition entries.
#'
#' @param icBuildingBlock A `BuildingBlock` object of type `Initial Conditions`.
#' The entries will be added to or set in this building block.
#' @param quantityPaths A list of full paths of the quantities (usually molecules). Should contain
#' all path elements and the molecule name, separated by `|`.
#' @param quantityValues A list of values for the quantities. The length of this list should be equal to the length of `quantityPaths`.
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
#' @param formula A list of `Formula` objects that will be set for the quantities.
#' Mixing of `formula` and `quantityValues` is not allowed. If `formula` is provided,
#' `quantityValues` should be `NULL`. The length of this list should be equal to the length of `quantityPaths`.
#'
#' @returns The updated `icBuildingBlock` object.
#' TBD: no return? To be consistent (or not confusing) with the extend functions?
#' @export
#'
#' @examples
setInitialConditions <- function(icBuildingBlock,
                                 quantityPaths,
                                 quantityValues,
                                 scaleDivisors = 1,
                                 isPresent = TRUE,
                                 negativeValuesAllowed = FALSE,
                                 formula = NULL){

  return(icBuildingBlock)
}

#' Delete entries from an Initial Conditions Building Block
#'
#' @param icBuildingBlock A `BuildingBlock` object of type `Initial Conditions`.
#' @param quantityPaths A list of full paths of the quantities (usually molecules) that
#' will be deleted. Entries not present in the provided BB are ignored. Should contain
#' all path elements and the molecule name, separated by `|`.
#'
#' @export
#'
#' @examples
deleteInitialConditions <- function(icBuildingBlock,
                                    quantityPaths){
}

#' Extend an Initial Conditions Building Block (BB) with new entries for molecules
#' from a molecules BB in all physical containers of a spatial structure BB.
#'
#' @param icBuildingBlock A `BuildingBlock` object of type `Initial Conditions`.
#' @param spatialStructureBB A `BuildingBlock` object of type `Spatial Structure`.
#' Entries will be created for the selected molecules in all physical containers of this
#' spatial structure.
#' @param moleculesBB A `BuildingBlock` object of type `Molecules`. The entries will be
#' created for all molecules from this building block, or for a subset of molecules
#' defined in the `moleculeNames` argument.
#' @param moleculeNames Optional list of molecule names. If provided, only the molecules
#' with these names will be added to the `icBuildingBlock`.
#'
#' @returns Path of entries added to the building block.
#' @export
#'
#' @examples
extendInitialConditions <- function(icBuildingBlock,
                                    spatialStructureBB,
                                    moleculesBB,
                                    moleculeNames = NULL){
}




#' Set or add parameter values to an existing Paramter Values building block.
#'
#' This functions allows adding or modifying parameter values entries.
#'
#' @param pvBuildingBlock A `BuildingBlock` object of type `Parameter Values`.
#' The entries will be added to or set in this building block.
#' @param quantityPaths A list of full paths of the quantities (usually parameters). Should contain
#' all path elements and the parameter name, separated by `|`.
#' @param formula A list of `Formula` objects that will be set for the quantities.
#' Mixing of `formula` and `quantityValues` is not allowed. If `formula` is provided,
#' `quantityValues` should be `NULL`. The length of this list should be equal to the length of `quantityPaths`.
#' @param dimensions A single dimension or a list of dimensions (string names)
#' of parameter values. Supported dimensions are listed in `ospDimension`. By default,
#' new entries get the `Dimensionless` dimension.
#'
#' @export
#'
#' @examples
setParameterValues <- function(pvBuildingBlock,
                                 quantityPaths,
                               dimensions = ospDimensions$Dimensionless,
                                 formula = NULL){

  return(pvBuildingBlock)
}

#' Delete entries from a Parameter Values Building Block
#'
#' @param pvBuildingBlock A `BuildingBlock` object of type `Parameter Values`.
#' @param quantityPaths A list of full paths of the quantities (usually parameters)
#' that will be deleted. Should contain all path elements and the parameter name, separated by `|`.
#' Entries not present in the provided BB are ignored.
#'
#' @export
#'
#' @examples
deleteParameterValues <- function(pvBuildingBlock,
                                    quantityPaths){
}

#' Extend a Parameter Values Building Block (BB) with local molecule parameters
#' for molecules from a molecules BB in all physical containers of a spatial structure BB.
#'
#' @param pvBuildingBlock A `BuildingBlock` object of type `Parameter Values`.
#' @param spatialStructureBB A `BuildingBlock` object of type `Spatial Structure`.
#' Entries will be created for local parameters of the selected molecules in all physical containers of this
#' spatial structure.
#' @param moleculesBB A `BuildingBlock` object of type `Molecules`. The entries will be
#' created for all molecules from this building block, or for a subset of molecules
#' defined in the `moleculeNames` argument.
#' @param moleculeNames Optional list of molecule names. If provided, only the molecules
#' with these names will be added to the `pvBuildingBlock`.
#'
#' @returns Path of entries added to the building block.
#' @export
#'
#' @examples
addLocalMoleculeParameters <- function(pvBuildingBlock,
                                    spatialStructureBB,
                                    moleculesBB,
                                    moleculeNames = NULL){
}

#' Extend a Parameter Values Building Block (BB) with protein expression parameters
#' for selected protein molecules in the selected organs.
#'
#' TBD: is a spatial structure required? Option 1 - define the organs as paths
#' Option 2 - provide a spatial structure and then define organ paths. Entries will
#' be created for all sub-organs.
#'
#' @param icBuildingBlock A `BuildingBlock` object of type `Initial Conditions`.
#' @param spatialStructureBB A `BuildingBlock` object of type `Spatial Structure`.
#' Entries will be created for the selected molecules in all physical containers of this
#' spatial structure.
#' @param moleculesBB A `BuildingBlock` object of type `Molecules`. The entries will be
#' created for all molecules from this building block, or for a subset of molecules
#' defined in the `moleculeNames` argument.
#' @param moleculeNames Optional list of molecule names. If provided, only the molecules
#' with these names will be added to the `icBuildingBlock`.
#'
#' @returns Path of entries added to the building block.
#' @export
#'
#' @examples
addProteinExpressionToParameterValuesBB <- function(pvBuildingBlock,
                                    spatialStructureBB,
                                    moleculesBB,
                                    moleculeNames = NULL){
}

