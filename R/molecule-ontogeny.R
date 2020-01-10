#' @title MoleculeOntogeny
#' @docType class
#' @description  Use when retrieving indvidual values using the createIndividualAlgorithm. This class is a simple pair {MoleculeName, Ontogeny}
#' allowing the user to retrieve potential ontogeny values.
#'
#' @field molecule Name of the molecule in the model
#' @field ontogeny Name of the ontogeny to use for the molecule
#' @format NULL
#' @export
MoleculeOntogeny <- R6::R6Class(
  "MoleculeOntogeny",
  inherit = Printable,
  public = list(
    molecule = NULL,
    ontogeny = NULL,
    initialize = function(molecule=NULL, ontogeny = NULL) {
      self$molecule <- molecule
      self$ontogeny <- ontogeny
    },
    print = function(...) {
      private$printClass()
      private$printLine("Molecule", self$molecule)
      private$printLine("Ontogeny", self$ontogeny)
      invisible(self)
    },
    printMoleculeOntogeny = function() {
      private$printLine(paste("Molecule", self$molecule, "with ontogeny", self$ontogeny))
    }
  )
)
