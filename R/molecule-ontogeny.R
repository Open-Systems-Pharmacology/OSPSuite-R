#' @title MoleculeOntogeny
#' @docType class
#' @description  Use when retrieving indvidual values using the createIndividualAlgorithm. This class is a simple pair {MoleculeName, Ontogeny}
#' allowing the user to retrieve potential ontogeny values.
#'
#' @field molecule Name of the molecule in the model
#' @field ontogeny Name of the ontogeny to use for the molecule
#' @format NULL
#' @export
MoleculeOntogeny<- R6::R6Class(
  "MoleculeOntogeny",
  inherit = DotNetWrapper,
  active = list(
    molecule = function(value) {
      private$wrapProperty("Molecule", value)
    },
    ontogeny = function(value) {
      private$wrapProperty("Ontogeny", value)
    }
  ),
  public = list(
    initialize = function() {
      ref <- rClr::clrNew("PKSim.R.Domain.MoleculeOntogeny")
      super$initialize(ref)
    },
    print = function(...) {
      private$printClass()
      private$printLine("Molecule", self$molecule)
      private$printLine("Ontogeny", self$ontogeny)
      invisible(self)
    }
  )
)
