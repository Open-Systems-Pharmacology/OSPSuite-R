download.file("https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/pk-sim/artifacts/pk-sim-r-dependencies.zip",
              destfile = "pk-sim-r-dependencies.zip")

unzip("pk-sim-r-dependencies.zip", exdir = "inst/lib")

file.remove("pk-sim-r-dependencies.zip")
