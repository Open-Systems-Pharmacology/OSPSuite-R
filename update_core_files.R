unlink("inst/lib", recursive = TRUE, force = TRUE)

download.file(
  "https://nightly.link/Open-Systems-Pharmacology/PK-Sim/workflows/build-and-test/develop/PKSim%20R%20Dependencies.zip",
  destfile = "pk-sim-r-dependencies_artefact.zip"
)

unzip("pk-sim-r-dependencies_artefact.zip", setTimes = TRUE, overwrite = T)

unzip(
  "pk-sim-r-dependencies.zip",
  exdir = "inst/lib",
  overwrite = TRUE,
  setTimes = TRUE
)

file.remove("pk-sim-r-dependencies_artefact.zip", "pk-sim-r-dependencies.zip")

# Rename System.Data.SQLite.dll to System.Data.SQLite.others.dll
# This file will be renamed back to System.Data.SQLite.dll on package load
sqlite_dll_path <- file.path("inst/lib", "System.Data.SQLite.dll")
sqlite_others_path <- file.path("inst/lib", "System.Data.SQLite.others.dll")

if (file.exists(sqlite_dll_path)) {
  file.rename(sqlite_dll_path, sqlite_others_path)
  cat("Renamed System.Data.SQLite.dll to System.Data.SQLite.others.dll\n")
} else {
  stop("System.Data.SQLite.dll not found in inst/lib")
}
