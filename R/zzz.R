# nocov start
.onLoad <- function(...) {

  # Only for x64 bits packages.
  # This is required to avoid error when package is being checked on CI for x86
  is64 <- (.Machine$sizeof.pointer == 8)
  if (!is64) {
    return()
  }

  # Now verify that the package is running on R 64
  isR64 <- R.version$arch == "x86_64"
  if(!isR64){
    stop("64 bit version of R is required.")
  }

  .initPackage()
}
# nocov end
