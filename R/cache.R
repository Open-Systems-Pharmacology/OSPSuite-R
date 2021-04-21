#' @title Cache
#' @docType class
#' @description Structure for caching objects
#'
Cache <- R6::R6Class(
  "Cache",
  cloneable = FALSE,
  public = list(
    #' @field type Type of the object that are cached. All objects must be of the same type.
    type = NULL,
    #' @description
    #' Initialize a new instance of the class
    #' @param type  Optional type constraints for object stored in the cache.
    #' @return A new `Cache` object.
    initialize = function(type = NULL) {
      self$type <- type
      private$cachedObjects <- new.env(hash = TRUE, parent = emptyenv())
    },

    #' @description
    #' Clear cache
    reset = function() {
      private$cachedObjects <- new.env(hash = TRUE, parent = emptyenv())
    },

    #' @description
    #' Store the given value in cache with the given key
    #' @param key Key to use to store the `value` in the case
    #' @param value Value to store in the cache
    set = function(key, value) {
      key <- toString(key)
      private$validateObjectType(value)
      assign(key, value, envir = private$cachedObjects)
    },

    #' @description
    #' Check if an entry with the given `key` exists in cache
    #' @param key Key to check
    hasKey = function(key) {
      key <- toString(key)
      exists(key, envir = private$cachedObjects, inherits = FALSE)
    },

    #' @description
    #' Get cached object by key
    #' @param key Key to use to retrieve the value
    get = function(key) {
      key <- toString(key)
      base::get(key, envir = private$cachedObjects, inherits = FALSE)
    },

    #' @description
    #' Remove the value associated with the key from cache.
    #' @param key Key used to find the value to remove. If the key is not found, nothing happens.
    dropKey = function(key) {
      key <- toString(key)
      if (self$hasKey(key)) {
        rm(list = key, envir = private$cachedObjects, inherits = FALSE)
      }
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      cat("Cache for objects of type: ", self$type, "\n", sep = "")
      cat("Cached keys:\n")
      lapply(self$keys, function(key) {
        cat(key, "\n", sep = "")
      })
      invisible(self)
    }
  ),
  active = list(
    #' @field keys List all key in the cache
    keys = function(value) {
      if (missing(value)) {
        ls(private$cachedObjects)
      } else {
        stop(messages$errorPropertyReadOnly("keys"), call. = FALSE)
      }
    }
  ),
  private = list(
    # Environment holding cached objects.
    cachedObjects = NULL,

    # All objects within a cache environment should be of the same type.
    validateObjectType = function(object) {
      if (is.null(self$type)) {
        return()
      }
      if (isOfType(object, self$type)) {
        return()
      }
      # Name of the variable in the calling function
      objectName <- deparse(substitute(object))
      stop(messages$errorWrongCacheType(objectName, self$type))
    }
  )
)
