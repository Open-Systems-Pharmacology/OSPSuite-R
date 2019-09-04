toObjectType <- function(netObject, class) {
  sapply(netObject, function(x)
    class$new(x))
}
