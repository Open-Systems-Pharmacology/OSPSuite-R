#' @title IndividualBuildingBlock
#' @docType class
#' @description An `Individual` building block. Subclass of [BuildingBlock] that
#' exposes the demographic properties of the underlying individual as read-only
#' fields (species, population, gender, age, gestational age, height, weight).
#' @format NULL
#' @export
IndividualBuildingBlock <- R6::R6Class(
  "IndividualBuildingBlock",
  cloneable = FALSE,
  inherit = BuildingBlock,
  active = list(
    #' @field species Species of the individual. Read-only.
    species = function(value) {
      private$.readOnlyProperty(
        "species",
        value,
        private$.originDataValue("Species")
      )
    },
    #' @field population Population of the individual. May be `NULL` for
    #' non-human species. Read-only.
    population = function(value) {
      private$.readOnlyProperty(
        "population",
        value,
        private$.originDataValue("Population")
      )
    },
    #' @field gender Gender of the individual. Read-only.
    gender = function(value) {
      private$.readOnlyProperty(
        "gender",
        value,
        private$.originDataValue("Gender")
      )
    },
    #' @field age Age of the individual. Read-only.
    age = function(value) {
      private$.readOnlyProperty(
        "age",
        value,
        private$.originDataValue("Age")
      )
    },
    #' @field gestationalAge Gestational age of the individual. Read-only.
    gestationalAge = function(value) {
      private$.readOnlyProperty(
        "gestationalAge",
        value,
        private$.originDataValue("Gestational age")
      )
    },
    #' @field height Height of the individual. Read-only.
    height = function(value) {
      private$.readOnlyProperty(
        "height",
        value,
        private$.originDataValue("Height")
      )
    },
    #' @field weight Weight of the individual. Read-only.
    weight = function(value) {
      private$.readOnlyProperty(
        "weight",
        value,
        private$.originDataValue("Weight")
      )
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class.
    #' @param netObject Reference to the underlying `Individual` building block.
    #' @return A new `IndividualBuildingBlock` object.
    initialize = function(netObject) {
      super$initialize(netObject, type = BuildingBlockTypes$Individual)
    },

    #' @description
    #' Print the object to the console.
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "Name" = self$name,
        "Type" = self$type
      ))
      ospsuite.utils::ospPrintHeader("Origin data", level = 2)
      ospsuite.utils::ospPrintItems(list(
        "Species" = self$species,
        "Population" = self$population,
        "Gender" = self$gender,
        "Age" = self$age,
        "Gestational age" = self$gestationalAge,
        "Height" = self$height,
        "Weight" = self$weight
      ))
      invisible(self)
    }
  ),
  private = list(
    .originDataValue = function(name) {
      all <- self$get("OriginData")$get("All")
      for (entry in all) {
        if (identical(entry$get("Name"), name)) {
          return(entry$get("Value"))
        }
      }
      NULL
    }
  )
)
