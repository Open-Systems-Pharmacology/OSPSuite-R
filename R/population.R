#' @title Population
#' @docType class
#' @description  List of individuals used in a population simulation
#' @format NULL
Population <- R6::R6Class(
  "Population",
  inherit = DotNetWrapper,
  cloneable = FALSE,
  active = list(
    #' @field count the number of individual in the population
    count = function(value) {
      private$.wrapReadOnlyProperty("Count", value)
    },
    #' @field allCovariateNames the names of all covariates defined in the population
    allCovariateNames = function(value) {
      private$.readOnlyProperty("allCovariateNames", value, self$call("AllCovariatesNames"))
    },
    #' @field allParameterPaths the paths of all parameters defined in the population
    allParameterPaths = function(value) {
      private$.readOnlyProperty("allParameterPaths", value, self$call("AllParameterPaths"))
    },
    #' @field allIndividualIds Ids of individuals defined in the population
    allIndividualIds = function(value) {
      private$.readOnlyProperty("allIndividualIds", value, self$call("AllIndividualIds"))
    }
  ),
  public = list(
    #' @description
    #' Returns `TRUE` if the population has variability defined for `parameterOrPath` otherwise `FALSE`
    #' @param parameterOrPath Parameter instance of parameter path
    has = function(parameterOrPath) {
      parameterPath <- private$getPathFrom(parameterOrPath)
      self$call("Has", parameterPath)
    },
    #' @description
    #' Updates or adds the variability values in the population for `parameterOrPath`.
    #' @param parameterOrPath Parameter instance of parameter path.
    #' If an entry already exists for this parameter by path, its values be overwritten, otherwise it will be created.
    #' @param values double vector containing the value to set for the `parameterOrPath`
    setParameterValues = function(parameterOrPath, values) {
      parameterPath <- private$getPathFrom(parameterOrPath)
      validateIsNumeric(values)
      self$call("SetValues", parameterPath, values)
      invisible(self)
    },
    #' @description
    #' Returns the variability values defined in the population for `parameterOrPath`
    #' @param parameterOrPath Parameter instance of parameter path
    getParameterValues = function(parameterOrPath) {
      parameterPath <- private$getPathFrom(parameterOrPath)
      self$call("GetValues", parameterPath)
    },
    #' @description
    #' Returns the values defined in the population for the covariate named `covariateName`
    #' @param covariateName Name of covariate for which values should be retrieved
    getCovariateValues = function(covariateName) {
      self$call("GetCovariateValues", covariateName)
    },
    #' @description
    #' Returns the values defined in the population for the covariate named `covariateName` and individual with id `individualId`
    #' @param covariateName Name of covariate for which values should be retrieved
    #' @param individualId Id of individual for which the value for covariate `covariateName` should be retrieved
    getCovariateValue = function(covariateName, individualId) {
      self$call("CovariateValueFor", covariateName, as.integer(individualId))
    },
    #' @description
    #' Returns all values defined in the population the individual with id `individualId`
    #' @param individualId Id of individual for which all values should be returned
    getParameterValuesForIndividual = function(individualId) {
      .parameterValueListFrom(self$call("AllParameterValuesForIndividual", as.integer(individualId)))
    },
    #' @description
    #' Removes the value of a parameter by path
    #' @param parameterPath Path of the parameter values to remove
    remove = function(parameterPath) {
      self$call("Remove", parameterPath)
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list("Number of Individuals" = self$count))
    }
  ),
  private = list(
    getPathFrom = function(parameterOrPath) {
      validateIsOfType(parameterOrPath, c("character", "Parameter"))
      if (isOfType(parameterOrPath, "Parameter")) {
        return(parameterOrPath$path)
      }
      parameterOrPath
    }
  )
)
