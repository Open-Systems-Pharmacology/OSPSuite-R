#' @title Population
#' @docType class
#' @description  List of individuals used in a population simulation
#' @field count the number of individual in the population
#' @field allCovariateNames the names of all covariates defined in the population
#' @section Methods:
#' \describe{
#'   \item{has(parameterOrPath)}{Returns \code{TRUE} if the population has variability defined for \code{parameterOrPath} otherwise \code{FALSE}}
#'   \item{setValues(parameterOrPath, values)}{Adds or update the \code{values} for \code{parameterOrPath}}
#'   \item{getValues(parameterOrPath)}{Returns the variability values defined in the population for \code{parameterOrPath}}
#'   \item{getCovariateValues(covariateName)}{Returns the values defined in the population for \code{covariateName}}
#'   \item{getCovariateValue(covariateName, individualId)}{Returns the covariate value defined in the population for the covariate \code{covariateName} and individual with id\code{individualId}}
#'   }
#' @format NULL
Population <- R6::R6Class(
  "Population",
  inherit = DotNetWrapper,
  active = list(
    count = function(value) {
      private$wrapReadOnlyProperty("Count", value)
    },
    allCovariateNames = function(value) {
      private$readOnlyProperty("allCovariateNames", value, rClr::clrCall(self$ref, "AllCovariatesNames"))
    }
  ),
  public = list(
    has = function(parameterOrPath) {
      parameterPath <- private$getPathFrom(parameterOrPath)
      rClr::clrCall(self$ref, "Has", parameterPath)
    },
    setValues = function(parameterOrPath, values) {
      parameterPath <- private$getPathFrom(parameterOrPath)
      validateIsNumeric(values)
      rClr::clrCall(self$ref, "SetValues", parameterPath, values)
      invisible(self)
    },
    getValues = function(parameterOrPath) {
      parameterPath <- private$getPathFrom(parameterOrPath)
      rClr::clrCall(self$ref, "GetValues", parameterPath)
    },
    getCovariateValues = function(covariateName) {
      rClr::clrCall(self$ref, "GetCovariateValues", covariateName)
    },
    getCovariateValue = function(covariateName, individualId) {
      rClr::clrCall(self$ref, "CovariateValueFor", covariateName, as.integer(individualId))
    },
    print = function(...) {
      private$printClass()
      private$printLine("Number of Individuals", self$count)
      invisible(self)
    }
  ),
  private = list(
    getPathFrom = function(parameterOrPath) {
      validateIsOfType(parameterOrPath, c("character", Parameter))
      if (isOfType(parameterOrPath, Parameter)) {
        return(parameterOrPath$consolidatePath)
      }
      parameterOrPath
    }
  )
)
