#' @title IndividualCharacteristics
#' @docType class
#' @description  Characteristics of an individual describing its origin
#'
#' @field species Specifies the species of the individual. It should be a species available in PK-Sim (see \code{Species})
#' @field population For a Human species, the population of intereset. It should be a population available in PK-Sim (see \code{HumanPopulation})
#' @field gender Gender of the individual. It should be defined for the species in PK-Sim  (see \code{Gender})
#' @field age Age of the individual as in instance of a \code{SnapshotParameter} (optional)
#' @field gestationalAge Gestational Age of the individual as in instance of a \code{SnapshotParameter} (optional)
#' @field weight Weight of the individual as in instance of a \code{SnapshotParameter} (optional)
#' @field height Height of the individual as in instance of a \code{SnapshotParameter} (optional)
#' @format NULL
#' @export
IndividualCharacteristics <- R6::R6Class(
  "IndividualCharacteristics",
  inherit = DotNetWrapper,
  active = list(
    species = function(value) {
      private$wrapProperty("Species", value)
    },
    population = function(value) {
      private$wrapProperty("Population", value)
    },
    gender = function(value) {
      private$wrapProperty("Gender", value)
    },
    age = function(value) {
      private$parameterProperty("Age", value)
    },
    gestationalAge = function(value) {
      private$parameterProperty("GestationalAge", value)
    },
    weight = function(value) {
      private$parameterProperty("Weight", value)
    },
    height = function(value) {
      private$parameterProperty("Height", value)
    }
  ),
  private = list(
    printParam = function(caption, param){
      if(is.null(param)){
        return()
      }
      param$printValue(caption)
    },
    parameterProperty = function(parameterName, value) {
      if (missing(value)) {
        SnapshotParameter$new(ref = rClr::clrGet(self$ref, parameterName))
      } else {
        rClr::clrSet(self$ref, name=parameterName, value = value$ref)
      }
    }
  ),
  public = list(
    initialize = function() {
      ref <- rClr::clrNew("PKSim.R.Domain.IndividualCharacteristics")
      super$initialize(ref)
    },
    print = function(...) {
      private$printClass()
      private$printLine("Species", self$species)
      private$printLine("Population", self$population)
      private$printLine("Gender", self$gender)
      private$printParam("Age", self$age)
      private$printParam("GestationalAge", self$gestationalAge)
      private$printParam("Weight", self$weight)
      private$printParam("Height", self$height)
      for(moleculeOntogeny in self$allMoleculeOntogenies()){
        moleculeOntogeny$printMoleculeOntogeny()
      }
      invisible(self)
    },
    addMoleculeOntogeny = function(moleculeOntogeny) {
      rClr::clrCall(self$ref, "AddMoleculeOntogeny", moleculeOntogeny$ref);
    },
    allMoleculeOntogenies = function() {
      toObjectType(rClr::clrGet(self$ref, "MoleculeOntogeniesAsArray"), MoleculeOntogeny)
    }
  )
)

#' @title SnapshotParameter
#' @docType class
#' @description  A parameter typically used in the definition of \code{OriginData} covariates (Height, Weight etc...)
#'
#' @field value Parameter value
#' @field unit Unit in which the value is defined
#' @format NULL
#' @export
SnapshotParameter<- R6::R6Class(
  "SnapshotParameter",
  inherit = DotNetWrapper,
  active = list(
    value = function(value) {
      private$wrapProperty("Value", value)
    },
    unit = function(value) {
      private$wrapProperty("Unit", value)
    }
  ),
  public = list(
    initialize = function(ref = NULL, value = NULL, unit = NULL) {
      validateIsNumeric(value, nullAllowed = TRUE)
      validateIsString(unit, nullAllowed = TRUE)
      ref <- ref %||% rClr::clrNew("PKSim.Core.Snapshots.Parameter")
      super$initialize(ref)
      # Because of weird issue with nullable value in rClr
      if(!is.null(value)){
        self$value <- value
      }
      if(!is.null(unit)){
        self$unit <- unit
      }
    },
    print = function(...) {
      private$printClass()
      private$printLine("Value", self$value)
      private$printLine("Unit", self$unit)
      invisible(self)
    },
    printValue = function(caption) {
      private$printLine(caption, paste0(formatNumerics(self$value), " [", self$unit, "]"))
    }
  )
)
