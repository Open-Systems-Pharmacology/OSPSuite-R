#' @title Population
#' @docType class
#' @description  List of individuals used in a population simulation
#' @field count the number of individual in the population
#' @field allCovariateNames the names of all covariates defined in the population
#' @section Methods:
#' \describe{
#'   \item{has(parameterPath)}{Returns \code{TRUE} if the population has variability defined for \code{parameterPath} otherwise \code{FALSE}}
#'   \item{setValues(parameterPath, values)}{Adds or update the \code{values} for \code{parameterPath}}
#'   \item{getValues(parameterPath)}{Returns the variability values defined in the population for \code{parameterPath}}
#'   \item{covariatesAt(individualId)}{Returns the \code{Covariates} defined for the individual with id \code{individualId}}
#'   }
#' @format NULL
Population <- R6::R6Class(
  "Population",
  inherit = DotNetWrapper,
  active = list(
    count = function(value) {
      private$wrapReadOnlyProperty("Count", value)
    },
    allCovariateNames = function(value){
      private$readOnlyProperty("allCovariateNames", value,  rClr::clrCall(self$ref, "AllCovariatesNames"))
    }
  ),
  public = list(
    has = function(parameterPath) {
      validateIsString(parameterPath)
      rClr::clrCall(self$ref, "Has", parameterPath)
    },
    setValues = function(parameterPath, values) {
      validateIsString(parameterPath)
      validateIsNumeric(values)
      rClr::clrCall(self$ref, "SetValues", parameterPath, values)
      invisible(self)
    },
    getValues = function(parameterPath) {
      validateIsString(parameterPath)
      rClr::clrCall(self$ref, "GetValues", parameterPath)
    },
    covariatesAt = function(individualId) {
      Covariates$new(rClr::clrCall(self$ref, "CovariatesAt", as.integer(individualId)))
    },
    print = function(...) {
      private$printClass()
      private$printLine("Number of Individuals", self$count)
      invisible(self)
    }
  )
)
