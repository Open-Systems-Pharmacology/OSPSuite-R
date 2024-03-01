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
      private$wrapIntegerProperty("NumberOfIndividuals", value)
    },
    #' @field proportionOfFemales Proportion of female in the population
    proportionOfFemales = function(value) {
      private$wrapIntegerProperty("ProportionOfFemales", value)
    },
    #' @field species Specifies the species of the individual. It should be a species available in PK-Sim (see `Species`)
    species = function(value) {
      private$wrapProperty("Species", value)
    },
    #' @field population For a Human species, the population of interest. It should be a population available in PK-Sim (see `HumanPopulation`)
    population = function(value) {
      private$wrapProperty("Population", value, shouldSetNull = FALSE)
    },
    #' @field age Age range of the population as in instance of a `ParameterRange` (optional)
    age = function(value) {
      private$parameterRangeProperty("Age", value)
    },
    #' @field gestationalAge Gestational Age range of the population as in instance of a `ParameterRange` (optional)
    gestationalAge = function(value) {
      private$parameterRangeProperty("GestationalAge", value)
    },
    #' @field weight Weight range of the population as in instance of a `ParameterRange` (optional)
    weight = function(value) {
      private$parameterRangeProperty("Weight", value)
    },
    #' @field height Height range of the population as in instance of a `ParameterRange` (optional)
    height = function(value) {
      private$parameterRangeProperty("Height", value)
    },
    #' @field BMI BMI range of the population as in instance of a `ParameterRange` (optional)
    BMI = function(value) {
      private$parameterRangeProperty("BMI", value)
    },
    #' @field allMoleculeOntogenies All molecule ontogenies defined for this individual characteristics.
    allMoleculeOntogenies = function(value) {
      private$readOnlyProperty("allMoleculeOntogenies", value, private$.moleculeOntogenies)
    },
    #' @field seed Seed used to generate the population
    seed = function(value) {
      private$wrapNullableIntegerProperty("Seed", value)
    }
  ),
  private = list(
    .moleculeOntogenies = NULL,
    printRange = function(caption, range) {
      if (is.null(range)) {
        return()
      }
      range$printValue(caption)
    },
    parameterRangeProperty = function(parameterName, value) {
      if (missing(value)) {
        ParameterRange$new(ref = rSharp::clrGet(self$ref, parameterName))
      } else {
        if (is.null(value)) {
          return()
        }
        rSharp::clrSet(self$ref, name = parameterName, value = value$ref)
      }
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @return A new `PopulationCharacteristics` object.
    initialize = function() {
      ref <- rSharp::clrNew("PKSim.R.Domain.PopulationCharacteristics")
      super$initialize(ref)
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Species", self$species)
      private$printLine("Population", self$population)
      private$printLine("Number of individuals", self$numberOfIndividuals)
      private$printLine("Proportion of females", self$proportionOfFemales)
      private$printRange("Age", self$age)
      private$printRange("Gestational age", self$gestationalAge)
      private$printRange("Weight", self$weight)
      private$printRange("Height", self$height)
      private$printRange("BMI", self$BMI)

      for (moleculeOntogeny in self$allMoleculeOntogenies) {
        moleculeOntogeny$printMoleculeOntogeny()
      }
      if (!is.null(self$seed)) {
        private$printLine("Seed", self$seed)
      }
      invisible(self)
    },

    #' @description
    #' Add a molecule ontogeny `MoleculeOntogeny` to the individual characteristics
    #' @param moleculeOntogeny Molecule ontogeny to add
    addMoleculeOntogeny = function(moleculeOntogeny) {
      validateIsOfType(moleculeOntogeny, "MoleculeOntogeny")
      private$.moleculeOntogenies <- c(private$.moleculeOntogenies, moleculeOntogeny)
      netMoleculeOntogeny <- rSharp::clrNew("PKSim.R.Domain.MoleculeOntogeny")
      rSharp::clrSet(netMoleculeOntogeny, "Molecule", moleculeOntogeny$molecule)
      rSharp::clrSet(netMoleculeOntogeny, "Ontogeny", moleculeOntogeny$ontogeny)
      rSharp::clrCall(self$ref, "AddMoleculeOntogeny", netMoleculeOntogeny)
    }
  )
)
