unlink("inst/lib", recursive = TRUE, force = TRUE)

download.file("https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/pk-sim/artifacts/pk-sim-r-dependencies.zip",
              destfile = "pk-sim-r-dependencies.zip")

unzip("pk-sim-r-dependencies.zip",
      exdir = "inst/lib",
      overwrite =  TRUE,
      setTimes = TRUE)

file.remove("pk-sim-r-dependencies.zip")
