# Delete existing lib directory
unlink("inst/lib", recursive = TRUE, force = TRUE)

# Download PK-Sim R Dependencies artifact from GitHub Actions
download.file(
  "https://nightly.link/Open-Systems-Pharmacology/PK-Sim/workflows/build-and-test/develop/PKSim%20R%20Dependencies.zip",
  destfile = "pk-sim-r-dependencies_artifact.zip"
)

# Unzip the downloaded artifact to extract the inner zip file
# use setTimes = TRUE to preserve timestamps
unzip("pk-sim-r-dependencies_artifact.zip", setTimes = TRUE, overwrite = T)

unzip(
  "pk-sim-r-dependencies.zip",
  exdir = "inst/lib",
  overwrite = TRUE,
  setTimes = TRUE
)

# Remove downloaded artifact and zip file
file.remove("pk-sim-r-dependencies_artifact.zip", "pk-sim-r-dependencies.zip")

# Prepare macOS arm64 files
lib_dir <- "inst/lib"

# Rename System.Data.SQLite.dll to System.Data.SQLite.windows_linux.dll
# Will be renamed back to System.Data.SQLite.dll during installation on windows and linux
file.rename(
  file.path(lib_dir, "System.Data.SQLite.dll"),
  file.path(lib_dir, "System.Data.SQLite.windows_linux.dll")
)

# Copy architecture-specific native libraries to generic names
native_libs <- c(
  "libOSPSuite.FuncParserNative",
  "libOSPSuite.SimModelNative",
  "libOSPSuite.SimModelSolver_CVODES"
)

arm64_natives <- file.path(
  lib_dir,
  paste(native_libs, "Arm64.dylib", sep = ".")
)
for (arm64_native in arm64_natives) {
  if (file.exists(arm64_native)) {
    target_file <- sub(".Arm64", "", arm64_native)
    file.rename(arm64_native, target_file)
  }
}
