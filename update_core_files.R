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
