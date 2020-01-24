#' @title IndividualCharacteristics
#' @docType class
#' @description  Characteristics of an individual describing its origin
#'
#' @format NULL
#' @export
IndividualCharacteristics <- R6::R6Class(
  "IndividualCharacteristics",
  inherit = DotNetWrapper,
  active = list(
    #' @field species Specifies the species of the individual. It should be a species available in PK-Sim (see \code{Species})
    species = function(value) {
      private$wrapProperty("Species", value)
    },
    #' @field population For a Human species, the population of intereset. It should be a population available in PK-Sim (see \code{HumanPopulation})
    population = function(value) {
      private$wrapProperty("Population", value, shouldSetNull = FALSE)
    },
    #' @field gender Gender of the individual. It should be defined for the species in PK-Sim  (see \code{Gender})
    gender = function(value) {
      private$wrapProperty("Gender", value, shouldSetNull = FALSE)
    },
    #' @field age Age of the individual as in instance of a \code{SnapshotParameter} (optional)
    age = function(value) {
      private$parameterProperty("Age", value)
    },
    #' @field gestationalAge Gestational Age of the individual as in instance of a \code{SnapshotParameter} (optional)
    gestationalAge = function(value) {
      private$parameterProperty("GestationalAge", value)
    },
    #' @field weight Weight of the individual as in instance of a \code{SnapshotParameter} (optional)
    weight = function(value) {
      private$parameterProperty("Weight", value)
    },
    #' @field height Height of the individual as in instance of a \code{SnapshotParameter} (optional)
    height = function(value) {
      private$parameterProperty("Height", value)
    },
    #' @field allMoleculeOntogenies All molecule ontogenies defined for this individual characteristics.
    allMoleculeOntogenies = function(value) {
      private$readOnlyProperty('allMoleculeOntogenies', value, private$.moleculeOntogenies)
    }
  ),
  private = list(
    .moleculeOntogenies = NULL,

    printParam = function(caption, param) {
      if (is.null(param)) {
        return()
      }
      param$printValue(caption)
    },
    parameterProperty = function(parameterName, value) {
      if (missing(value)) {
        SnapshotParameter$new(ref = rClr::clrGet(self$ref, parameterName))
      } else {
        if (is.null(value)) {
          return()
        }
        rClr::clrSet(self$ref, name = parameterName, value = value$ref)
      }
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @return A new `IndividualCharacteristics` object.
    initialize = function() {
      ref <- rClr::clrNew("PKSim.R.Domain.IndividualCharacteristics")
      super$initialize(ref)
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Species", self$species)
      private$printLine("Population", self$population)
      private$printLine("Gender", self$gender)
      private$printParam("Age", self$age)
      private$printParam("GestationalAge", self$gestationalAge)
      private$printParam("Weight", self$weight)
      private$printParam("Height", self$height)
      for (moleculeOntogeny in self$allMoleculeOntogenies()) {
        moleculeOntogeny$printMoleculeOntogeny()
      }
      invisible(self)
    },

    #' @description
    #' Add a molecule ontogeny `MoleculeOntogeny` to the individual characteristics
    #' @param moleculeOntogeny Molecule ontogeny to add
    addMoleculeOntogeny = function(moleculeOntogeny) {
      validateIsOfType(moleculeOntogeny, MoleculeOntogeny)
      private$.moleculeOntogenies <- c(private$.moleculeOntogenies, moleculeOntogeny)
      netMoleculeOntogeny <- rClr::clrNew("PKSim.R.Domain.MoleculeOntogeny")
      rClr::clrSet(netMoleculeOntogeny, "Molecule", moleculeOntogeny$molecule)
      rClr::clrSet(netMoleculeOntogeny, "Ontogeny", moleculeOntogeny$ontogeny)
      rClr::clrCall(self$ref, "AddMoleculeOntogeny", netMoleculeOntogeny)
    }
  )
)

#' @title SnapshotParameter
#' @docType class
#' @description  A parameter typically used in the definition of \code{OriginData} covariates (Height, Weight etc...)
#'
#' @format NULL
#' @export
SnapshotParameter <- R6::R6Class(
  "SnapshotParameter",
  inherit = DotNetWrapper,
  active = list(
    #' @field value Parameter value
    value = function(value) {
      private$wrapProperty("Value", value)
    },
    #' @field unit Unit in which the value is defined
    unit = function(value) {
      private$wrapProperty("Unit", value)
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param ref Optional .NET reference object. If not defined, a new instance will be created
    #' @param value Optional value of the parameter.
    #' @param unit Optional unit of the value specified.
    #' @return A new `SnapshotParameter` object.
    initialize = function(ref = NULL, value = NULL, unit = NULL) {
      validateIsNumeric(value, nullAllowed = TRUE)
      validateIsString(unit, nullAllowed = TRUE)
      ref <- ref %||% rClr::clrNew("PKSim.Core.Snapshots.Parameter")
      super$initialize(ref)
      # Because of weird issue with nullable value in rClr
      if (!is.null(value)) {
        self$value <- value
      }
      if (!is.null(unit)) {
        self$unit <- unit
      }
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Value", self$value)
      private$printLine("Unit", self$unit)
      invisible(self)
    },
    #' @description
    #' Print the the parameter in one line
    #' @param caption Caption to display before the value of the parameter
    printValue = function(caption) {
      private$printLine(caption, paste0(formatNumerics(self$value), " [", self$unit, "]"))
    }
  )
)
