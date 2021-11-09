#' @title Printable
#' @docType class
#' @description  Base class that implements some basic properties for printing to console
#'
Printable <- R6::R6Class(
  "Printable",
  private = list(
    printLine = function(entry, value = NULL, addTab = TRUE) {
      entries <- paste0(entry, ":", sep = "")

      # helps to visually distinguish class name from its entries
      if (addTab) {
        entries <- c("  ", entries)
      }

      entries <- c(entries, value)
      entries <- c(entries, "\n")
      cat(entries, sep = " ")
      invisible(self)
    },
    printClass = function() {
      cat(class(self)[1], ": \n", sep = "")
    }
  )
)
