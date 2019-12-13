#' @title OriginData
#' @docType class
#' @description  Simulation Interval (typically associated with an instace of \code{OutputSchema})
#'
#' @field startTime Start time of interval (instance of \code{Parameter})
#' @field endTime End time of interval (instance of \code{Parameter})
#' @field resolution Resolution of interval in pts/min (instance of \code{Parameter})
#' @format NULL
#' @export
OriginData <- R6::R6Class(
  "OriginData",
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
      ref <- rClr::clrNew("PKSim.Core.Snapshots.OriginData")
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
      invisible(self)
    }
  )
)

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
