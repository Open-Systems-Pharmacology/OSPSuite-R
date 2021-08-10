#' @title DataImporterConfiguration
#' @docType class
#' @description Configuration of data import from excel or csv files. To be used with #TODO
#' @export
#' @format NULL
DataImporterConfiguration <- R6::R6Class(
  "DataSet",
  inherit = DotNetWrapper,
  cloneable = TRUE,
  active = list(
    #' @field timeColumn Name of the column for time values
    timeColumn = function(value){
      if (missing(value)) {
        #TODO get time column name
        return()
      }
      #TODO set time column name
    },

    #' @field timeUnit If \code{timeUnitFromColumn} is \code{FALSE}, unit of the values in time column
    #' If \code{timeUnitFromColumn} is \code{TRUE}, name of the column with the unit of the values in time column
    timeUnit = function(value){
      if (missing(value)) {
        #TODO get time column unit
        return()
      }
      #TODO set time column unit
    },

    #' @field timeUnitFromColumn If \code{TRUE}, units of the values in time column
    #' are defined in the column \code{timeUnit}. If \code{FALSE}, the unit is defined by
    #' \code{timeUnit}.
    timeUnitFromColumn = function(value){
    }

    #' @field measurementColumn Name of the column for measurement values
    measurementColumn = function(value){
      if (missing(value)) {
        #TODO get measurement column name
        return()
      }
      #TODO set measurement column name
    },

    #' @field measurementDimension If \code{measurementUnitFromColumn} is \code{FALSE}, dimension of the values in measurement column
    #' If \code{measurementUnitFromColumn} is \code{TRUE}, the dimension is guessed from the unit defined in the column \code{measurementUnit}
    #'
    measurementDimension = function(value){
      if (missing(value)) {
        #TODO get measurement column dimension
        return()
      }
      #TODO set measurement column dimension
    },

    #' @field measurementUnit If \code{measurementUnitFromColumn} is \code{FALSE}, unit of the values in measurement column
    #' If \code{measurementUnitFromColumn} is \code{TRUE}, name of the column with the unit of the values in measurement column
    measurementUnit = function(value){
      if (missing(value)) {
        #TODO get measurement column unit
        return()
      }
      #TODO set measurement column unit
    },

    #' @field measurementUnitFromColumn If \code{TRUE}, units of the values in measurement column
    #' are defined in the column \code{measurementUnit}. If \code{FALSE}, the unit is defined by
    #' \code{measurementUnit}.
    measurementUnitFromColumn = function(value){
    },

    #' @field errorColumn Name of the column for measurement error values
    errorColumn = function(value){
      if (missing(value)) {
        #TODO get error column name
        return()
      }
      #TODO set error column name
    },

    #' @field errorColumnUnit Unit of the values in measurement column
    errorColumnUnit = function(value){
      if (missing(value)) {
        #TODO get error column unit
        return()
      }
      #TODO set error column unit
      #TODO from column?
    },

    #' @field errorColumnType Type of the measurement error values. See enum \code{DataErrorType}
    #' for possible values
    errorColumnType = function(value){
      if (missing(value)) {
        #TODO get error column unit
        return()
      }
      #TODO set error column unit
      #TODO from column?
    },

    #' @field speciesColumn Name of the column to be mapped to meta data \code{Species}
    #' for possible values
    speciesColumn = function(value){
      if (missing(value)) {
        #TODO get
        return()
      }
      #TODO set
    },

    #' @field organColumn Name of the column to be mapped to meta data \code{Organ}
    #' for possible values
    organColumn = function(value){
      if (missing(value)) {
        #TODO get
        return()
      }
      #TODO set
    },


    #' @field compartmentColumn Name of the column to be mapped to meta data \code{compartment}
    #' for possible values
    compartmentColumn = function(value){
      if (missing(value)) {
        #TODO get
        return()
      }
      #TODO set
    },

    #' @field studyIdColumn Name of the column to be mapped to meta data \code{StudyId}
    #' for possible values
    studyIdColumn = function(value){
      if (missing(value)) {
        #TODO get
        return()
      }
      #TODO set
    },


    #' @field studyIdColumn Name of the column to be mapped to meta data \code{StudyId}
    #' for possible values
    studyIdColumn = function(value){
      if (missing(value)) {
        #TODO get
        return()
      }
      #TODO set
    },

    #' @field studyIdColumn Name of the column to be mapped to meta data \code{StudyId}
    #' for possible values
    studyIdColumn = function(value){
      if (missing(value)) {
        #TODO get
        return()
      }
      #TODO set
    },

    #' @field studyIdColumn Name of the column to be mapped to meta data \code{StudyId}
    #' for possible values
    studyIdColumn = function(value){
      if (missing(value)) {
        #TODO get
        return()
      }
      #TODO set
    },

    #' @field studyIdColumn Name of the column to be mapped to meta data \code{StudyId}
    #' for possible values
    studyIdColumn = function(value){
      if (missing(value)) {
        #TODO get
        return()
      }
      #TODO set
    },

    #' @field studyIdColumn Name of the column to be mapped to meta data \code{StudyId}
    #' for possible values
    studyIdColumn = function(value){
      if (missing(value)) {
        #TODO get
        return()
      }
      #TODO set
    },

    #' @field studyIdColumn Name of the column to be mapped to meta data \code{StudyId}
    #' for possible values
    studyIdColumn = function(value){
      if (missing(value)) {
        #TODO get
        return()
      }
      #TODO set
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param configurationFilePath Path to the XML file with stored configuration
    #' (e.g. create in PK-Sim or MoBi).
    #' If \code{NULL} (default), an empty configuration is created.
    #' @return A new `DataImporterConfiguration` object.
    initialize = function(configurationFilePath = NULL) {
      task <- getNetTask("DataImporterTask")

      if (is.null(configurationFilePath)){
        ref <- rClr::clrCall(task, "CreateConfiguration")
      } else {
        validateIsString(configurationFilePath)
        ref <- rClr::clrCall(task, "GetConfiguration", configurationFilePath)
      }
      super$initialize(ref)
      private$.dataImporterTask <- task
    },

    #' @description
    #' Save configuration to a XML file that can be used in PKSim/MoBi
    #' @param filePath Path (incl. file name) to the location where the configuration
    #' will be exported to.
    saveConfiguration = function(filePath){
      validateIsString(filePath)
      filePath <- expandPath(filePath)

      rClr::clrCall(private$.dataImporterTask, "SaveConfiguration", self$ref, filePath)
    }



    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      invisible(self)
    }
  ),
  private = list(
    .dataImporterTask = NULL


  )
)
