#' @title IndividualCharacteristics
#' @docType class
#' @description  Characteristics of an individual describing its origin
#'
#' @format NULL
#' @export
IndividualCharacteristics <- R6::R6Class(
  "IndividualCharacteristics",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  active = list(
    #' @field species Specifies the species of the individual. It should be a species available in PK-Sim (see `Species`)
    species = function(value) {
      private$.wrapProperty("Species", value)
    },
    #' @field population For a Human species, the population of interest. It should be a population available in PK-Sim (see `HumanPopulation`)
    population = function(value) {
      private$.wrapProperty("Population", value, shouldSetNull = FALSE)
    },
    #' @field gender Gender of the individual. It should be defined for the species in PK-Sim  (see `Gender`)
    gender = function(value) {
      private$.wrapProperty("Gender", value, shouldSetNull = FALSE)
    },
    #' @field age Age of the individual as in instance of a `SnapshotParameter` (optional)
    age = function(value) {
      private$parameterProperty("Age", value)
    },
    #' @field gestationalAge Gestational Age of the individual as in instance of a `SnapshotParameter` (optional)
    gestationalAge = function(value) {
      private$parameterProperty("GestationalAge", value)
    },
    #' @field weight Weight of the individual as in instance of a `SnapshotParameter` (optional)
    weight = function(value) {
      private$parameterProperty("Weight", value)
    },
    #' @field height Height of the individual as in instance of a `SnapshotParameter` (optional)
    height = function(value) {
      private$parameterProperty("Height", value)
    },
    #' @field allMoleculeOntogenies All molecule ontogenies defined for this individual characteristics.
    allMoleculeOntogenies = function(value) {
      private$.readOnlyProperty("allMoleculeOntogenies", value, private$.moleculeOntogenies)
    },
    #' @field seed Seed used to generate the population
    seed = function(value) {
      private$.wrapProperty("Seed", value, asInteger = TRUE)
    }
  ),
  private = list(
    .moleculeOntogenies = NULL,
    parameterProperty = function(parameterName, value) {
      if (missing(value)) {
        SnapshotParameter$new(netObject = self$get(parameterName))
      } else {
        if (is.null(value)) {
          return()
        }
        self$set(name = parameterName, value = value)
      }
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @return A new `IndividualCharacteristics` object.
    initialize = function() {
      # Assuming that if this function is called directly, PK-Sim was either initialized already
      # or should be initialized automatically
      initPKSim()
      netObject <- rSharp::newObjectFromName("PKSim.R.Domain.IndividualCharacteristics")
      super$initialize(netObject)
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "Species" = self$species,
        "Population" = self$population,
        "Gender" = self$gender
      ), print_empty = TRUE)
      ospsuite.utils::ospPrintItems(list(
        "Age" = self$age$getPrintValue(),
        "Gestational age" = self$gestationalAge$getPrintValue(),
        "Weight" = self$weight$getPrintValue(),
        "Height" = self$height$getPrintValue()
      ))
      ospsuite.utils::ospPrintItems(list("Seed" = self$seed))

      if (!is.null(self$allMoleculeOntogenies)) {
        ospsuite.utils::ospPrintHeader("Molecule Ontogenies", level = 2)
        for (ontogeny in self$allMoleculeOntogenies) {
          ontogeny$printMoleculeOntogeny()
        }
      }
    },

    #' @description
    #' Add a molecule ontogeny `MoleculeOntogeny` to the individual characteristics
    #' @param moleculeOntogeny Molecule ontogeny to add
    addMoleculeOntogeny = function(moleculeOntogeny) {
      validateIsOfType(moleculeOntogeny, "MoleculeOntogeny")
      private$.moleculeOntogenies <- c(private$.moleculeOntogenies, moleculeOntogeny)
      netMoleculeOntogeny <- rSharp::newObjectFromName("PKSim.R.Domain.MoleculeOntogeny")
      netMoleculeOntogeny$set("Molecule", moleculeOntogeny$molecule)
      netMoleculeOntogeny$set("Ontogeny", moleculeOntogeny$ontogeny)
      self$call("AddMoleculeOntogeny", netMoleculeOntogeny)
    }
  )
)
