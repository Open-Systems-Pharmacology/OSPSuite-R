#' @title PopulationCharacteristics
#' @docType class
#' @description  Characteristics of a population used for population creation
#'
#' @format NULL
#' @export
PopulationCharacteristics <- R6::R6Class(
  "PopulationCharacteristics",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  active = list(
    #' @field numberOfIndividuals Number of individuals in the population
    numberOfIndividuals = function(value) {
      private$.wrapProperty("NumberOfIndividuals", value, asInteger = TRUE)
    },
    #' @field proportionOfFemales Proportion of female in the population
    proportionOfFemales = function(value) {
      private$.wrapProperty("ProportionOfFemales", value, asInteger = TRUE)
    },
    #' @field species Specifies the species of the individual. It should be a species available in PK-Sim (see `Species`)
    species = function(value) {
      private$.wrapProperty("Species", value)
    },
    #' @field population For a Human species, the population of interest. It should be a population available in PK-Sim (see `HumanPopulation`)
    population = function(value) {
      private$.wrapProperty("Population", value, shouldSetNull = FALSE)
    },
    #' @field age Age range of the population as in instance of a `ParameterRange` (optional)
    age = function(value) {
      private$.parameterRangeProperty("Age", value)
    },
    #' @field gestationalAge Gestational Age range of the population as in instance of a `ParameterRange` (optional)
    gestationalAge = function(value) {
      private$.parameterRangeProperty("GestationalAge", value)
    },
    #' @field weight Weight range of the population as in instance of a `ParameterRange` (optional)
    weight = function(value) {
      private$.parameterRangeProperty("Weight", value)
    },
    #' @field height Height range of the population as in instance of a `ParameterRange` (optional)
    height = function(value) {
      private$.parameterRangeProperty("Height", value)
    },
    #' @field BMI BMI range of the population as in instance of a `ParameterRange` (optional)
    BMI = function(value) {
      private$.parameterRangeProperty("BMI", value)
    },
    #' @field allMoleculeOntogenies All molecule ontogenies defined for this population characteristics.
    allMoleculeOntogenies = function(value) {
      private$.readOnlyProperty(
        "allMoleculeOntogenies",
        value,
        private$.moleculeOntogenies
      )
    },
    #' @field seed Seed used to generate the population
    seed = function(value) {
      private$.wrapProperty("Seed", value, asInteger = TRUE)
    }
  ),
  private = list(
    .moleculeOntogenies = NULL,
    .parameterRangeProperty = function(parameterName, value) {
      if (missing(value)) {
        ParameterRange$new(netObject = self$get(parameterName))
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
    #' @return A new `PopulationCharacteristics` object.
    initialize = function() {
      # Assuming that if this function is called directly, PK-Sim was either initialized already
      # or should be initialized automatically
      initPKSim()
      netObject <- rSharp::newObjectFromName(
        "PKSim.R.Domain.PopulationCharacteristics"
      )
      super$initialize(netObject)
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(
        list(
          "Species" = self$species,
          "Population" = self$population,
          "Number of individuals" = self$numberOfIndividuals,
          "Proportion of females" = self$proportionOfFemales,
          "Age" = self$age$getPrintValue(),
          "Gestational age" = self$gestationalAge$getPrintValue(),
          "Weight" = self$weight$getPrintValue(),
          "Height" = self$height$getPrintValue(),
          "BMI" = self$BMI$getPrintValue()
        ),
        print_empty = TRUE
      )
      ospsuite.utils::ospPrintItems(list("Seed" = self$seed))

      if (!is.null(self$allMoleculeOntogenies)) {
        ospsuite.utils::ospPrintHeader("Molecule Ontogenies")
        for (moleculeOntogeny in self$allMoleculeOntogenies) {
          moleculeOntogeny$printMoleculeOntogeny()
        }
      }
      invisible(self)
    },

    #' @description
    #' Add a molecule ontogeny `MoleculeOntogeny` to the individual characteristics
    #' @param moleculeOntogeny Molecule ontogeny to add
    addMoleculeOntogeny = function(moleculeOntogeny) {
      validateIsOfType(moleculeOntogeny, "MoleculeOntogeny")
      private$.moleculeOntogenies <- c(
        private$.moleculeOntogenies,
        moleculeOntogeny
      )
      netMoleculeOntogeny <- rSharp::newObjectFromName(
        "OSPSuite.R.Domain.MoleculeOntogeny"
      )
      netMoleculeOntogeny$set("Molecule", moleculeOntogeny$molecule)
      netMoleculeOntogeny$set("Ontogeny", moleculeOntogeny$ontogeny)
      invisible(self$call("AddMoleculeOntogeny", netMoleculeOntogeny))
    }
  )
)
