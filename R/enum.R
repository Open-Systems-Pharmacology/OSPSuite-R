
#' Create an enumeration to be used instead of arbitrary values in code.
#'
#' @param enumValues Values to use for the enum
#'
#' @return the Enum created
#' @examples
#' \dontrun{
#' # Without predefined values
#' Color <- enum(c("Red", "Blue", "Green"))
#' myColor <- Color$Red
#'
#' # With predefined values
#' Symbol <- enum(c(Diamond = 1, Triangle = 2, Circle = 2))
#' mySymbol <- Symbol$Diamond
#' }
enum <- function(enumValues) {
  myEnum <- as.list(enumValues)
  enumNames <- names(myEnum)
  if (is.null(enumNames)) {
    names(myEnum) <- myEnum
  } else if (any(enumNames == "")) {
    stop(messages$errorEnumNotAllNames)
  }
  return(myEnum)
}

#' Get the key mapped to the given value in an \code{enum}
#'
#' @param enum The enum where the key-value pair is stored
#' @param value The value that is mapped to the key
#'
#' @return Key under which the value is stored. If the value is not in the enum, \code{NULL} is returned
#' @export
getEnumKey <- function(enum, value) {
  output <- names(which(enum == value))
  if (length(output) == 0) {
    return(NULL)
  }
  return(output)
}

#' Return the value that is stored under the given key. If the key is not present, an error is thrown.
#'
#' @param enum enum that contains the key-value pair
#' @param key key under which the value is stored
#'
#' @return Value that is assigned to \code{key}
#' @export
enumGetValue <- function(enum, key) {
  if (!enumHasKey(key, enum)) {
    stop(messages$errorKeyNotInEnum(key))
  }

  return(enum[[key]])
}

#' Return all keys of an enum
#'
#' @param enum \code{enum} containing the keys
#'
#' @return List of key names
#' @export
enumKeys <- function(enum) {
  names(enum)
}

#' Check if an enum has a certain key.
#'
#' @param key Key to check for
#' @param enum Enum where to look for the key
#'
#' @return TRUE if a key-value pair for \code{key} exists, FALSE otherwise
#' @export
enumHasKey <- function(key, enum) {
  return(any(enumKeys(enum) == key))
}

#' Add a new key-value pairs to an enum.
#'
#' @param keys Keys of the values to be added
#' @param values Values to be added
#' @param enum enum the key-value pairs should be added to.
#' WARNING: the original object is not modified!
#' @param overwrite if TRUE and a value with any of the given \code{keys} exists,
#' it will be overwritten with the new value. Otherwise, an error is thrown. Default is FALSE.
#'
#' @return Enum with added key-value pair.
#' @export
#'
#' @examples
#' \dontrun{
#' myEnum <- enum(c(a = "b"))
#' myEnum <- enumPut("c", "d", myEnum)
#' myEnum <- enumPut(c("c", "d", "g"), c(12, 2, "a"), myEnum, overwrite = TRUE)
#' }
enumPut <- function(keys, values, enum, overwrite = FALSE) {
  validateIsSameLength(keys, values)

  for (i in seq_along(keys)) {
    if (enumHasKey(keys[[i]], enum) && !overwrite) {
      stop(messages$errorKeyInEnumPresent(keys[[i]]))
    }
    enum[[keys[[i]]]] <- values[[i]]
  }

  return(enum)
}

#' Remove an entry from the enum.
#'
#' @param keys Key(s) of entries to be removed from the enum
#' @param enum Enum from which the entries to be removed
#' WARNING: the original object is not modified!
#'
#' @return Enum without the removed entries
#' @export
enumRemove <- function(keys, enum) {
  for (key in keys) {
    enum[[key]] <- NULL
  }

  return(enum)
}

#' Return the values stored in an enum
#'
#' @param enum \code{enum} containing the values
#'
#' @return List of values stored in the \code{enum}
#' @export
enumValues <- function(enum) {
  unlist(enum, use.names = FALSE)
}
