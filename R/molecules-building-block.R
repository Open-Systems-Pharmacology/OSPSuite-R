#' @title MoleculesBuildingBlock
#' @docType class
#' @description A `Molecules` building block. Subclass of [BuildingBlock].
#' @format NULL
#' @export
MoleculesBuildingBlock <- R6::R6Class(
  "MoleculesBuildingBlock",
  cloneable = FALSE,
  inherit = BuildingBlock,
  public = list(
    #' @description
    #' Initialize a new instance of the class.
    #'
    #' @param netObject Reference to the underlying `Molecules` building block.
    #' @return A new `MoleculesBuildingBlock` object.
    initialize = function(netObject) {
      super$initialize(netObject, type = BuildingBlockTypes$Molecules)
    },

    #' @description
    #' Returns the names of all molecules defined in the building block.
    #' @return Character vector of molecule names.
    allMoleculeNames = function() {
      .moleculesTask()$call("AllMoleculeNames", self)
    },

    #' @description
    #' Returns the names of all floating molecules in the building block
    #' (molecules with `IsFloating = TRUE`, e.g. drugs, metabolites).
    #' @return Character vector of molecule names.
    allFloatingMoleculeNames = function() {
      .moleculesTask()$call("AllFloatingMoleculeNames", self)
    },

    #' @description
    #' Returns the names of all stationary molecules in the building block
    #' (molecules with `IsFloating = FALSE`, e.g. enzymes, transporters).
    #' @return Character vector of molecule names.
    allStationaryMoleculeNames = function() {
      .moleculesTask()$call("AllStationaryMoleculeNames", self)
    },

    #' @description
    #' Returns the names of all molecules of the given type. Pass
    #' `MoleculeType$Protein` to obtain all proteins (the union of
    #' `Enzyme`, `Transporter`, and `Binding Partner`).
    #' @param moleculeType One of the values defined in the `MoleculeType` enum
    #' (e.g. `Drug`, `Metabolite`, `Enzyme`, `Transporter`, `Binding Partner`,
    #' `Complex`, `Protein`).
    #' @return Character vector of molecule names.
    allMoleculeNamesOfType = function(moleculeType) {
      validateEnumValue(moleculeType, MoleculeType)
      .moleculesTask()$call(
        "AllMoleculeNamesOfType",
        self,
        .quantityTypeNetObject(moleculeType)
      )
    },

    #' @description
    #' Returns the names of all xenobiotic floating molecules in the building
    #' block (e.g. drugs, drug complexes).
    #' @return Character vector of molecule names.
    allXenobioticFloatingMoleculeNames = function() {
      .moleculesTask()$call("AllXenobioticFloatingMoleculeNames", self)
    },

    #' @description
    #' Returns the names of all endogenous stationary molecules in the
    #' building block (e.g. enzymes, transporters, other proteins).
    #' @return Character vector of molecule names.
    allEndogenousStationaryMoleculeNames = function() {
      .moleculesTask()$call("AllEndogenousStationaryMoleculeNames", self)
    },

    #' @description
    #' Returns the type of a molecule by name as it is recorded in the
    #' building block (e.g. `"Drug"`, `"Enzyme"`, `"Transporter"`,
    #' `"Binding Partner"`). Throws an error if the molecule is not present.
    #' @param moleculeName Name of the molecule to look up.
    #' @return Character string with the molecule type.
    moleculeTypeFor = function(moleculeName) {
      validateIsString(moleculeName)
      netType <- .moleculesTask()$call("MoleculeTypeFor", self, moleculeName)
      .netMoleculeTypeToDisplay(netType)
    }
  )
)

#' Cached `MoleculesTask` resolver.
#' @keywords internal
.moleculesTask <- function() {
  .getMoBiTaskFromCache("MoleculesTask")
}

#' Convert a `MoleculeType` integer flag value to the underlying
#' `OSPSuite.Core.Domain.QuantityType` enum object that the engine accepts as a
#' parameter.
#' @keywords internal
.quantityTypeNetObject <- function(moleculeType) {
  rSharp::callStatic(
    "System.Enum",
    "ToObject",
    rSharp::getType("OSPSuite.Core.Domain.QuantityType"),
    as.integer(moleculeType)
  )
}

#' Translate an internal `QuantityType` name to its user-facing label.
#' The internal `OtherProtein` flag is exposed as `"Binding Partner"`; all
#' other names pass through unchanged.
#' @keywords internal
.netMoleculeTypeToDisplay <- function(netType) {
  if (identical(netType, "OtherProtein")) "Binding Partner" else netType
}
