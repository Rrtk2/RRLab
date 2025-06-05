## Load libraryR from the package source tree
## Assume tests are run from within 'tests/testthat'
## Tests are run from tests/testthat, so go two levels up to package root
source(file.path('..', '..', 'RRLab', 'R', 'libraryR.R'), local = TRUE)
