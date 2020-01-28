#' @title MoleculeOntogeny
#' @docType class
#' @description  Use when retrieving indvidual values using the createIndividualAlgorithm. This class is a simple pair {MoleculeName, Ontogeny}
#' allowing the user to retrieve potential ontogeny values.
#'
#' @format NULL
#' @export
MoleculeOntogeny <- R6::R6Class(
  "MoleculeOntogeny",
  inherit = Printable,
  public = list(
    #' @field molecule Name of the molecule in the model
    molecule = NULL,

    #' @field ontogeny Name of the ontogeny to use for the molecule
    ontogeny = NULL,

    #' @description
    #' Initialize a new instance of the class
    #' @param molecule Optional molecule name
    #' @param ontogeny Optional ontogeny to use for the Molecule
    #' @return A new `MoleculeOntogeny` object.
    initialize = function(molecule = NULL, ontogeny = NULL) {
      self$molecule <- molecule
      self$ontogeny <- ontogeny
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Molecule", self$molecule)
      private$printLine("Ontogeny", self$ontogeny)
      invisible(self)
    },
    #' @description
    #' Print the `MoleculeOntogeny` on one line
    printMoleculeOntogeny = function() {
      private$printLine(paste("Molecule", self$molecule, "with ontogeny", self$ontogeny))
    }
  )
)
