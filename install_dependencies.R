install.packages(c('testthat', 'vdiffr', 'spelling','covr', 'showtext'), repos = 'http://cran.us.r-project.org')
install.packages('https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v0.9.1/rClr_0.9.1.zip', repos = NULL, type = 'binary')
download.file('https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/ospsuite-rutils/artifacts/ospsuite.utils.zip?pr=false', destfile = 'ospsuite.utils.zip', mode='wb'); install.packages('ospsuite.utils.zip', repos = NULL, type = 'binary')
download.file('https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/tlf-library/artifacts/tlf.zip?pr=false', destfile = 'tlf.zip', mode='wb');  install.packages('tlf.zip', repos = NULL, type = 'binary')
