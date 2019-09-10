#' @title Cache
#' @docType class
#' @description Structure for caching objects
#'
#' @field type Type of the object that are cached. All objects must be of the same type.
#' @field keys Keys of the cached objects
Cache <- R6::R6Class(
  "Cache",
  public = list(
    type = NULL,
    initialize = function(type) {
      self$type <- type
    },

    # Clear cache
    reset = function() {
      private$cachedObjects <- new.env(hash = TRUE, parent = emptyenv())
    },

    # Store the given value in cache with the given key
    set = function(key, value) {
      private$validateObjectType(value)
      assign(key, value, envir = private$cachedObjects)
    },

    # Check if an entry with the given key exists in cache
    hasKey = function(key) {
      return(exists(key, envir = private$cachedObjects, inherits = FALSE))
    },

    # Get cached object by key
    get = function(key) {
      base::get(key, envir = private$cachedObjects, inherits = FALSE)
    },

    # Remove the value associated with the key from cache. If the key is not found,
    # nothing happens.
    dropKey = function(key) {
      if (self$hasKey(key)) {
        rm(list = key, envir = private$cachedObjects, inherits = FALSE)
      }
    },

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
    # List all key in the cache
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
    cachedObjects = new.env(hash = TRUE, parent = emptyenv()),

    # All objects within a cache environment should be of the same type.
    validateObjectType = function(object) {
      if (!isOfType(object, self$type)) {
        # Name of the variable in the calling function
        objectName <- deparse(substitute(object))
        stop(messages$errorWrongCacheType(objectName, self$type))
      }
    }
  )
)
