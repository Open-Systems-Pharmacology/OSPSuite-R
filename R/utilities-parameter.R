toParameters <- function(netParams) {
  sapply(netParams, function(p)
    Parameter$new(p))
}
