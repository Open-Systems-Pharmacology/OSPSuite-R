#' @title MoleculeOntogeny
#' @docType class
#' @description  Use when retrieving individual values using the createIndividualAlgorithm. This class is a simple pair (MoleculeName, Ontogeny)
#' allowing the user to retrieve potential ontogeny values.
#'
#' @format NULL
#' @export
MoleculeOntogeny <- R6::R6Class(
  "MoleculeOntogeny",
  cloneable = FALSE,
  public = list(
    #' @field molecule Name of the molecule in the model
    molecule = NULL,

    #' @field ontogeny Name of the ontogeny to use for the molecule
    ontogeny = NULL,

    #' @description
    #' Initialize a new instance of the class
    #' @param molecule molecule name
    #' @param ontogeny ontogeny to use for the Molecule (one of StandardOntogeny)
    #' @return A new `MoleculeOntogeny` object.
    initialize = function(molecule, ontogeny) {
      validateIsString(molecule)
      validateEnumValue(ontogeny, StandardOntogeny)
      self$molecule <- molecule
      self$ontogeny <- ontogeny
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::osp_print_class(self)
      ospsuite.utils::osp_print_items(list(
        "Molecule" = self$molecule,
        "Ontogeny" = self$ontogeny
      ))
    },
    #' @description
    #' Print the `MoleculeOntogeny` on one line
    printMoleculeOntogeny = function() {
      print(paste0("Molecule '", self$molecule, "' with ontogeny '", self$ontogeny, "'"))
    }
  )
)
