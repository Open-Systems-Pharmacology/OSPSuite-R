#' @title Printable
#' @docType class
#' @description  Base class that implements some basic functionalities for printing to console
#'
Printable <- R6::R6Class(
  "Printable",
  private = list(
    printLine = function(entry, value = NULL) {
      entries <- c("  ", entry)
      if (!is.null(value)) {
        entries <- c(entries, ": ", value)
      }

      entries <- c(entries, "\n")
      cat(entries, sep = "")

      invisible(self)
    },

    printClass = function() {
      cat(class(self)[1], ": \n", sep = "")
    }
  )
)
